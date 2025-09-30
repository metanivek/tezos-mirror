(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Tezlink_imports
open Tezlink_mock

(* Module importing, amending, and converting, protocol types. Those types
   might be difficult to actually build, so we define conversion function from
   local types to protocol types. *)
module Protocol_types = struct
  module Alpha_context = Imported_protocol.Alpha_context
  module Raw_level = Alpha_context.Raw_level

  module Cycle = struct
    include Alpha_context.Cycle

    (* This function is copied from [cycle_repr.ml] because it is not exposed
       in [alpha_context.mli]. *)
    let of_int32_exn i =
      if Compare.Int32.(i >= 0l) then add root (Int32.to_int i)
      else invalid_arg "Cycle_repr.of_int32_exn"
  end

  module Level = struct
    open Tezos_types

    type t = Alpha_context.Level.t

    let encoding = Alpha_context.Level.encoding

    (** The sole purpose of this encoding is to reflect as closely as possible
          the encoding of Alpha_context.Level.t, so it can be used to convert to
          that type, with a serialization pass. This is necessary only because
          there isn't a simple way to build that type. *)
    let conversion_encoding =
      let open Data_encoding in
      conv
        (fun ({level; cycle; cycle_position} : level) ->
          ( Raw_level.of_int32_exn level,
            Int32.pred level,
            Cycle.of_int32_exn cycle,
            cycle_position,
            false ))
        (fun ( level,
               _level_position,
               cycle,
               cycle_position,
               _expected_commitment )
           ->
          let level = Raw_level.to_int32 level in
          let cycle = Cycle.to_int32 cycle in
          {level; cycle; cycle_position})
        (obj5
           (req "level" Raw_level.encoding)
           (req "level_position" int32)
           (req "cycle" Cycle.encoding)
           (req "cycle_position" int32)
           (req "expected_commitment" bool))

    let convert : level -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"level"
        ~dst:encoding
        ~src:conversion_encoding
  end

  module Counter = struct
    type t = Alpha_context.Manager_counter.t

    let encoding = Alpha_context.Manager_counter.encoding_for_RPCs

    let of_z : Z.t -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"counter"
        ~dst:encoding
        ~src:Data_encoding.n
  end
end

module type BLOCK_SERVICES = sig
  include Tezos_shell_services.Block_services.S

  val deserialize_operations :
    chain_id:Chain_id.t -> bytes -> operation list tzresult

  val mock_block_header_data :
    chain_id:Chain_id.t -> Proto.block_header_data tzresult

  val mock_block_header_metadata :
    Tezos_types.level -> Proto.block_header_metadata tzresult
end

(** We add to Imported_protocol the mocked protocol data used in headers *)
module Imported_protocol = struct
  include Imported_protocol

  let contents : Block_header_repr.contents =
    {
      payload_hash = Block_payload_hash.zero;
      payload_round = Round_repr.zero;
      seed_nonce_hash = None;
      proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '\000';
      per_block_votes =
        {
          liquidity_baking_vote = Per_block_vote_pass;
          adaptive_issuance_vote = Per_block_vote_pass;
        };
    }

  let signature : Alpha_context.signature =
    Unknown (Bytes.make Tezos_crypto.Signature.Ed25519.size '\000')

  let mock_protocol_data : Block_header_repr.protocol_data =
    {contents; signature}
end

type error +=
  | Deserialize_operation of string * string * string
  | Deserialize_operations_header of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.deserialize_operation"
    ~title:"Failed to deserialize operation informations"
    ~description:
      "Failed to deserialize an operation informations (operation and receipt)."
    ~pp:(fun ppf (op, error, raw_receipt) ->
      Format.fprintf
        ppf
        "Failed to deserialize the information of operation %s with error %s \
         (raw data: [0x%s])"
        op
        error
        raw_receipt)
    Data_encoding.(
      obj3 (req "op" string) (req "error_msg" string) (req "raw_receipt" string))
    (function
      | Deserialize_operation (op, error, raw_receipt) ->
          Some (op, error, raw_receipt)
      | _ -> None)
    (fun (op, error, raw_receipt) ->
      Deserialize_operation (op, error, raw_receipt))

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.deserialize_operations_header"
    ~title:"Failed to deserialize the headers of operations"
    ~description:"Failed to deserialize the headers of a set of operations."
    ~pp:(fun ppf error ->
      Format.fprintf
        ppf
        "Failed to deserialize the headers of a set of operations: %s"
        error)
    Data_encoding.(obj1 (req "error_msg" string))
    (function Deserialize_operations_header error -> Some error | _ -> None)
    (fun error -> Deserialize_operations_header error)

module Make_block_service
    (Proto : Tezos_shell_services.Block_services.PROTO)
    (Next_proto : Tezos_shell_services.Block_services.PROTO) =
struct
  include Tezos_shell_services.Block_services.Make (Proto) (Next_proto)

  let deserialize_operations_header bytes =
    let open Result_syntax in
    let operations =
      Data_encoding.Binary.to_bytes_exn Data_encoding.bytes bytes
    in
    Data_encoding.Binary.of_bytes
      Data_encoding.(
        list
          (tup3
             Operation_hash.encoding
             Tezos_base.Operation.shell_header_encoding
             bytes))
      operations
    |> Result.map_error_e (fun read_error ->
           tzfail
           @@ Deserialize_operations_header
                (Format.asprintf
                   "%a"
                   Data_encoding.Binary.pp_read_error
                   read_error))

  let deserialize_operations ~chain_id bytes =
    let open Result_syntax in
    let* operations = deserialize_operations_header bytes in
    List.map_e
      (fun (hash, (shell_header : Tezos_base.Operation.shell_header), op_receipt)
         ->
        let* protocol_data, receipt =
          Data_encoding.Binary.of_bytes
            Proto.operation_data_and_receipt_encoding
            op_receipt
          |> Result.map_error_e (fun read_error ->
                 tzfail
                 @@ Deserialize_operation
                      ( Operation_hash.to_b58check hash,
                        Format.asprintf
                          "%a"
                          Data_encoding.Binary.pp_read_error
                          read_error,
                        Hex.show (Hex.of_bytes op_receipt) ))
        in
        return
          ({
             chain_id;
             hash;
             shell = shell_header;
             protocol_data;
             receipt = Receipt receipt;
           }
            : operation))
      operations
end

module Current_block_services = struct
  include Make_block_service (Imported_protocol) (Imported_protocol)

  let voting_period ~cycles_per_voting_period ~position ~level_info =
    let open Tezos_types in
    (* There's a certain amount of cycle in each period, to get the index
       of the current, we just divide the current number of cycle by the
       number of cycle for a period (floor div) *)
    let index = Int32.div level_info.cycle cycles_per_voting_period in
    (* The start_position can be determined based on the current position
       given in parameter. *)
    let start_position = Int32.(sub (sub level_info.level 1l) position) in
    Voting_period.
      {index; kind = Alpha_context.Voting_period.Proposal; start_position}

  let voting_period_info ~block_per_cycle ~cycles_per_voting_period ~level_info
      =
    let open Tezos_types in
    let open Result_syntax in
    (* The number of cycles in the current period is the remainder of the
       Euclidean division between the current cycle index the number of
       cycles in a voting period. *)
    let number_of_cycle_in_period =
      Int32.rem level_info.cycle cycles_per_voting_period
    in
    (* During a voting period, the position parameter is the number of block
       produced since the beginning of the period. We can deduce it based on
       the number of cycles in the current period. *)
    let position =
      Int32.(
        add
          (mul number_of_cycle_in_period block_per_cycle)
          level_info.cycle_position)
    in
    let* voting_period =
      Voting_period.convert
        (voting_period ~cycles_per_voting_period ~position ~level_info)
    in
    (* The number of block remaining is deducted from the current position
       and the number of cycles needed to complete a period. *)
    let remaining =
      Int32.(sub (mul cycles_per_voting_period block_per_cycle) position)
    in
    let voting_period_info =
      Imported_protocol.Alpha_context.Voting_period.
        {voting_period; position; remaining}
    in
    return voting_period_info

  let mock_block_header_data ~chain_id:_ : Proto.block_header_data tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_data"
      ~dst:Proto.block_header_data_encoding
      ~src:Imported_protocol.Block_header_repr.protocol_data_encoding
      Imported_protocol.mock_protocol_data

  let mock_block_header_metadata level_info =
    let open Imported_protocol.Apply_results in
    let open Result_syntax in
    let proposer =
      Imported_protocol.Alpha_context.Consensus_key.
        {
          delegate = Tezlink_mock.baker_account.public_key_hash;
          consensus_pkh = Tezlink_mock.baker_account.public_key_hash;
        }
    in
    let balance_updates =
      let amount = Alpha_context.Tez.of_mutez_exn 0L in
      Tezlink_mock.balance_udpdate_rewards
        ~baker:Tezlink_mock.baker_account.public_key_hash
        ~amount
    in

    let constant = Tezlink_constants.all_constants.parametric in
    let* voting_period_info =
      voting_period_info
        ~block_per_cycle:constant.blocks_per_cycle
        ~cycles_per_voting_period:constant.cycles_per_voting_period
        ~level_info
    in
    let* level_info = Protocol_types.Level.convert level_info in
    return
      {
        proposer;
        baker = proposer;
        level_info;
        voting_period_info;
        nonce_hash = None;
        consumed_gas = Alpha_context.Gas.Arith.zero;
        deactivated = [];
        balance_updates;
        liquidity_baking_toggle_ema =
          Imported_protocol.Alpha_context.Per_block_votes
          .Liquidity_baking_toggle_EMA
          .zero;
        adaptive_issuance_vote_ema =
          Imported_protocol.Alpha_context.Per_block_votes
          .Adaptive_issuance_launch_EMA
          .zero;
        adaptive_issuance_launch_cycle = None;
        implicit_operations_results = [];
        dal_attestation = Imported_protocol.Alpha_context.Dal.Attestation.empty;
      }

  let transfer_receipt account balance : operation_receipt =
    let open Alpha_context.Receipt in
    let open Imported_protocol.Apply_results in
    let contract = Tezos_types.Contract.of_implicit account in
    let mint =
      item
        (Contract
           (Alpha_context.Contract.Implicit Tezlink_mock.faucet_public_key_hash))
        (Debited balance)
        Block_application
    in
    let bootstrap =
      item (Contract contract) (Credited balance) Block_application
    in
    let balance_updates = [mint; bootstrap] in
    let operation_result =
      Transaction_result
        (Transaction_to_contract_result
           {
             storage = None;
             lazy_storage_diff = None;
             balance_updates;
             ticket_receipt = [];
             originated_contracts = [];
             consumed_gas = Alpha_context.Gas.Arith.zero;
             storage_size = Z.zero;
             paid_storage_size_diff = Z.zero;
             allocated_destination_contract = false;
           })
    in
    let internal_operation_results = [] in
    let operation_result =
      Imported_protocol.Apply_operation_result.Applied operation_result
    in
    let contents =
      Single_result
        (Manager_operation_result
           {balance_updates = []; operation_result; internal_operation_results})
    in
    Receipt (Operation_metadata {contents})

  let faucet_counter () : Alpha_context.Manager_counter.t tzresult =
    Tezos_types.convert_using_serialization
      ~name:"faucet_counter"
      ~src:Data_encoding.z
      ~dst:Tezlink_imports.Alpha_context.Manager_counter.encoding_for_RPCs
      Z.zero

  let create_transfer ~chain_id (account : Alpha_context.public_key_hash)
      (balance : Alpha_context.Tez.t) : operation tzresult =
    let open Result_syntax in
    let open Imported_protocol.Alpha_context in
    let operation =
      Transaction
        {
          amount = balance;
          parameters = Alpha_context.Script.unit_parameter;
          entrypoint = Entrypoint.default;
          destination = Implicit account;
        }
    in
    let* counter = faucet_counter () in
    let contents =
      Single
        (Manager_operation
           {
             source = Tezlink_mock.faucet_public_key_hash;
             fee = Tez.zero;
             counter;
             operation;
             gas_limit = Gas.Arith.zero;
             storage_limit = Z.zero;
           })
    in
    let signature = None in
    let protocol_data = Operation_data {contents; signature} in
    let shell : Tezos_base.Operation.shell_header =
      {branch = Tezos_crypto.Hashed.Block_hash.zero}
    in
    let receipt = transfer_receipt account balance in
    let hash = Operation_hash.zero in
    return {chain_id; hash; receipt; shell; protocol_data}

  let activate_bootstraps_with_transfers ~chain_id
      (module Backend : Tezlink_backend_sig.S) =
    let open Lwt_result_syntax in
    let* bootstrap_accounts = Backend.bootstrap_accounts () in
    let*? ops =
      List.map_e
        (fun (account, balance) -> create_transfer ~chain_id account balance)
        bootstrap_accounts
    in
    return ops
end

module Zero_block_services = struct
  include Make_block_service (Zero_protocol) (Genesis_protocol)

  let mock_block_header_data ~chain_id:_ : Proto.block_header_data tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_data"
      ~dst:Proto.block_header_data_encoding
      ~src:Data_encoding.empty
      ()

  let mock_block_header_metadata _ : Proto.block_header_metadata tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_metadata"
      ~dst:Proto.block_header_metadata_encoding
      ~src:Data_encoding.empty
      ()
end

module Genesis_block_services = struct
  include Make_block_service (Genesis_protocol) (Imported_protocol)

  let mock_block_header_data ~chain_id : Proto.block_header_data tzresult =
    let parameter =
      Imported_protocol_parameters.Default_parameters.parameters_of_constants
        ~bootstrap_accounts:[Tezlink_mock.baker_account]
        Tezlink_constants.all_constants.parametric
    in
    let parameter_format_json =
      Imported_protocol_parameters.Default_parameters.json_of_parameters
        ~chain_id
        parameter
    in
    let protocol_parameters =
      Data_encoding.Binary.to_bytes_exn Data_encoding.json parameter_format_json
    in
    Result_syntax.return
      Genesis_protocol.
        {
          command =
            Genesis_protocol.Data.Command.Activate
              {
                protocol = Imported_protocol.hash;
                fitness = [];
                protocol_parameters;
              };
          signature = Signature.V0.zero;
        }

  let mock_block_header_metadata _ : Proto.block_header_metadata tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_metadata"
      ~dst:Proto.block_header_metadata_encoding
      ~src:Data_encoding.empty
      ()
end

(** [wrap conversion service_implementation] changes the output type
    of [service_implementation] using [conversion]. *)
let wrap conv impl p q i =
  let open Lwt_result_syntax in
  let* res = impl p q i in
  let*? result = conv res in
  return result

(** [import_service s] makes it possible to substitute new [prefix] and [param]
    types in the signature of the service [s]. *)
let import_service s = Tezos_rpc.Service.subst0 s

(** [import_service_with_arg s] makes it possible to substitute new [prefix]
    types in the signature of the service [s]. It also substitute the [param]
    but keeps the last arg of the n-uplet intact. *)
let import_service_with_arg s = Tezos_rpc.Service.subst1 s

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

type tezlink_rpc_context = {block : block; chain : chain}

(** Builds a [tezlink_rpc_context] from paths parameters. *)
let make_env (chain : chain) (block : block) : tezlink_rpc_context Lwt.t =
  Lwt.return {block; chain}

module Tezlink_protocols = struct
  module Shell_impl = Tezos_shell_services.Block_services

  type protocols = Shell_impl.protocols
end

module Adaptive_issuance_services = struct
  include Imported_protocol_plugin.Adaptive_issuance_services
  module Cycle = Protocol_types.Cycle
  module Tez = Tezos_types.Tez

  let dummy_reward i =
    {
      cycle = i;
      baking_reward_fixed_portion = Tez.one;
      baking_reward_bonus_per_slot = Tez.one;
      attesting_reward_per_slot = Tez.one;
      dal_attesting_reward_per_shard = Tez.one;
      seed_nonce_revelation_tip = Tez.one;
      vdf_revelation_tip = Tez.one;
    }

  let consensus_rights_delay =
    Tezlink_constants.all_constants.parametric.consensus_rights_delay

  let dummy_rewards current_cycle =
    List.init ~when_negative_length:[] (consensus_rights_delay + 1) (fun i ->
        dummy_reward Cycle.(add (of_int32_exn current_cycle) i))
end

let expected_issuance :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Adaptive_issuance_services.expected_rewards list )
    Tezos_rpc.Service.t =
  import_service Adaptive_issuance_services.S.expected_issuance

(* Raw services normally represents the context of the Tezos blockchain.
   But because context in Tezlink is different, we need to mock some rpcs
   to make Tzkt indexing works. *)
module Raw_services = struct
  let custom_root : tezlink_rpc_context Tezos_rpc.Path.context =
    Tezos_rpc.Path.(open_root / "context" / "raw" / "json")

  let cycle =
    let open Tezos_rpc in
    Service.get_service
      ~description:
        "Returns the cycle <i>. This RPC is a mock as there's no cycle notion \
         in Tezlink and doesn't represent what's in the context of a block"
      ~query:Query.empty
      ~output:Storage_repr.Cycle.sampler_encoding
      Imported_env.RPC_path.(
        custom_root / "cycle" /: Imported_protocol.Cycle_repr.rpc_arg)
end

(* This is where we import service declarations from the protocol. *)
module Protocol_plugin_services = Imported_protocol_plugin.RPC.S

type level_query = Protocol_plugin_services.level_query = {offset : int32}

let current_level :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      level_query,
      unit,
      Protocol_types.Level.t )
    Tezos_rpc.Service.t =
  import_service Protocol_plugin_services.current_level

let version :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      Tezos_version.Octez_node_version.t )
    Tezos_rpc.Service.t =
  Tezos_shell_services.Version_services.S.version

let protocols :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Tezlink_protocols.protocols )
    Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Shell_services.Blocks.S.protocols

(* Queries will be ignored for now. *)
let contract_info :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      Imported_protocol_plugin.Contract_services.S.normalize_types_query,
      unit,
      Imported_protocol_plugin.Contract_services.info )
    Tezos_rpc.Service.t =
  import_service_with_arg Imported_protocol_plugin.Contract_services.S.info

let list_entrypoints :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      Imported_protocol_plugin.Contract_services.S.normalize_types_query,
      unit,
      Imported_protocol.Michelson_v1_primitives.prim list list
      * (string * Alpha_context.Script.expr) list )
    Tezos_rpc.Service.t =
  import_service_with_arg
    Imported_protocol_plugin.Contract_services.S.list_entrypoints

let balance :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Tezos_types.Tez.t )
    Tezos_rpc.Service.t =
  import_service_with_arg Imported_protocol_plugin.Contract_services.S.balance

let manager_key :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Protocol_types.Alpha_context.public_key option )
    Tezos_rpc.Service.t =
  import_service_with_arg
    Imported_protocol_plugin.Contract_services.S.manager_key

let counter :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Protocol_types.Counter.t )
    Tezos_rpc.Service.t =
  import_service_with_arg Imported_protocol_plugin.Contract_services.S.counter

let constants :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Protocol_types.Alpha_context.Constants.t )
    Tezos_rpc.Service.t =
  import_service Imported_protocol_plugin.Constants_services.S.all

let hash :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Block_hash.t )
    Tezos_rpc.Service.t =
  import_service Current_block_services.S.hash

let chain_id :
    ([`GET], chain, chain, unit, unit, Chain_id.t) Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Chain_services.S.chain_id

let operation_hashes :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Operation_hash.t list list )
    Tezos_rpc.Service.t =
  import_service Current_block_services.S.Operation_hashes.operation_hashes

let operation :
    ( [`GET],
      tezlink_rpc_context,
      (tezlink_rpc_context * int) * int,
      < force_metadata : bool
      ; metadata : [`Always | `Never] option
      ; version : Tezos_shell_services.Block_services.version >,
      unit,
      Tezos_shell_services.Block_services.version
      * Current_block_services.operation )
    Tezos_rpc.Service.t =
  Tezos_rpc.Service.subst2 Current_block_services.S.Operations.operation

let bootstrapped :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      Block_hash.t * Time.Protocol.t )
    Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Monitor_services.S.bootstrapped

let is_bootstrapped :
    ( [`GET],
      chain,
      chain,
      unit,
      unit,
      bool
      * Tezos_shell_services.Chain_validator_worker_state.synchronisation_status
    )
    Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Chain_services.S.is_bootstrapped

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7965 *)
(* We need a proper implementation *)
let simulate_operation :
    ( [`POST],
      tezlink_rpc_context,
      tezlink_rpc_context,
      < successor_level : bool
      ; version : Imported_protocol_plugin.RPC.version option >,
      int32 option * Alpha_context.packed_operation * Chain_id.t * int,
      Alpha_context.packed_protocol_data * Imported_protocol.operation_receipt
    )
    Tezos_rpc.Service.t =
  import_service Imported_protocol_plugin.RPC.Scripts.S.simulate_operation

let monitor_heads :
    ( [`GET],
      unit,
      unit * chain,
      < protocols : Protocol_hash.t list
      ; next_protocols : Protocol_hash.t list >,
      unit,
      Block_hash.t * Block_header.t )
    Tezos_rpc.Service.t =
  Tezos_shell_services.Monitor_services.S.heads

let get_storage_normalized :
    ( [`POST],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      Imported_protocol.Script_ir_unparser.unparsing_mode,
      Alpha_context.Script.expr option )
    Tezos_rpc.Service.t =
  import_service_with_arg
    Imported_protocol_plugin.RPC.Contract.S.get_storage_normalized

let injection_operation :
    ( [`POST],
      unit,
      unit,
      < async : bool ; chain : chain option >,
      bytes,
      Operation_hash.t )
    Tezos_rpc.Service.t =
  Tezos_shell_services.Injection_services.S.operation

let preapply_operations :
    ( [`POST],
      tezlink_rpc_context,
      tezlink_rpc_context,
      < version : Tezlink_protocols.Shell_impl.version >,
      Protocol_types.Alpha_context.packed_operation list,
      Tezlink_protocols.Shell_impl.version
      * (Protocol_types.Alpha_context.packed_protocol_data
        * Imported_protocol.operation_receipt)
        list )
    Tezos_rpc.Service.t =
  import_service Current_block_services.S.Helpers.Preapply.operations

let raw_json_cycle :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Imported_protocol.Cycle_repr.t,
      unit,
      unit,
      Storage_repr.Cycle.storage_cycle )
    Tezos_rpc.Service.t =
  import_service_with_arg Raw_services.cycle

module Forge = struct
  let operations :
      ( [`POST],
        tezlink_rpc_context,
        tezlink_rpc_context,
        unit,
        Operation.shell_header * Alpha_context.packed_contents_list,
        bytes )
      Tezos_rpc.Service.t =
    import_service Imported_protocol_plugin.RPC.Forge.S.operations
end
