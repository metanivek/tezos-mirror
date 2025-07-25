(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Client_proto_args
open Baking_errors
module Events = Baking_events.Commands
module Command_run = Octez_agnostic_baker.Command_run

let pidfile_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun _ s -> return s))

let http_headers_env_variable =
  "TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS"

let http_headers =
  match Sys.getenv_opt http_headers_env_variable with
  | None -> None
  | Some contents ->
      let lines = String.split_on_char '\n' contents in
      Some
        (List.fold_left
           (fun acc line ->
             match String.index_opt line ':' with
             | None ->
                 invalid_arg
                   (Printf.sprintf
                      "Http headers: invalid %s environment variable, missing \
                       colon"
                      http_headers_env_variable)
             | Some pos ->
                 let header = String.trim (String.sub line 0 pos) in
                 let header = String.lowercase_ascii header in
                 if header <> "host" then
                   invalid_arg
                     (Printf.sprintf
                        "Http headers: invalid %s environment variable, only \
                         'host' headers are supported"
                        http_headers_env_variable) ;
                 let value =
                   String.trim
                     (String.sub line (pos + 1) (String.length line - pos - 1))
                 in
                 (header, value) :: acc)
           []
           lines)

let operations_arg =
  Tezos_clic.arg
    ~long:"operations-pool"
    ~placeholder:"file|uri"
    ~doc:
      (Printf.sprintf
         "When specified, the baker will try to fetch operations from this \
          file (or uri) and to include retrieved operations in the block. The \
          expected format of the contents is a list of operations [ \
          alpha.operation ].  Environment variable '%s' may also be specified \
          to add headers to the requests (only 'host' headers are supported). \
          If the resource cannot be retrieved, e.g., if the file is absent, \
          unreadable, or the web service returns a 404 error, the resource is \
          simply ignored."
         http_headers_env_variable)
    (Tezos_clic.map_parameter
       ~f:(fun uri ->
         let open Baking_configuration in
         match Uri.scheme uri with
         | Some "http" | Some "https" ->
             Operations_source.(Remote {uri; http_headers})
         | None | Some _ ->
             (* acts as if it were file even though it might no be *)
             Operations_source.(Local {filename = Uri.to_string uri}))
       uri_parameter)

let data_dir_path_arg =
  Tezos_clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:
      "When specified, the client will load and read the context located in \
       the local data directory at the provided path, in order to build the \
       block, instead of relying on the 'preapply' RPC."
    string_parameter

let force_apply_from_round_arg =
  Tezos_clic.arg
    ~long:"force-apply-from-round"
    ~placeholder:"round"
    ~doc:
      "Force the baker to not only validate but also apply operations starting \
       from the specified round."
    int_parameter

let attestation_force_switch_arg =
  Tezos_clic.switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "Disable consistency, injection and double signature checks for \
       (pre)attestations."
    ()

let do_not_monitor_node_mempool_arg =
  Tezos_clic.switch
    ~long:"ignore-node-mempool"
    ~doc:
      "Ignore mempool operations from the node and do not subsequently monitor \
       them. Use in conjunction with --operations option to restrict the \
       observed operations to those of the mempool file."
    ()

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let per_block_vote_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter
    ~autocomplete:(fun _ctxt -> return ["on"; "off"; "pass"])
    (let open Protocol.Alpha_context.Per_block_votes in
     fun _ctxt -> function
       | "on" -> return Per_block_vote_on
       | "off" -> return Per_block_vote_off
       | "pass" -> return Per_block_vote_pass
       | s ->
           failwith
             "unexpected vote: %s, expected either \"on\", \"off\", or \
              \"pass\"."
             s)

let liquidity_baking_toggle_vote_arg =
  Tezos_clic.arg
    ~doc:
      "Vote to continue or end the liquidity baking subsidy. The possible \
       values for this option are: \"off\" to request ending the subsidy, \
       \"on\" to request continuing or restarting the subsidy, and \"pass\" to \
       abstain. Note that this \"option\" is mandatory!"
    ~long:"liquidity-baking-toggle-vote"
    ~placeholder:"vote"
    per_block_vote_parameter

let adaptive_issuance_vote_arg =
  Tezos_clic.arg
    ~doc:
      "Vote to adopt or not the adaptive issuance feature. The possible values \
       for this option are: \"off\" to request not activating it, \"on\" to \
       request activating it, and \"pass\" to abstain. If you do not vote, \
       default value is \"pass\"."
    ~long:"adaptive-issuance-vote"
    ~placeholder:"vote"
    per_block_vote_parameter

let state_recorder_switch_arg =
  let open Lwt_result_syntax in
  let open Baking_configuration in
  Tezos_clic.map_arg
    ~f:(fun _cctxt flag -> if flag then return Filesystem else return Memory)
    (Tezos_clic.switch
       ~long:"record-state"
       ~doc:
         "If record-state flag is set, the baker saves all its internal \
          consensus state in the filesystem, otherwise just in memory."
       ())

let node_version_check_bypass_arg =
  Tezos_clic.switch
    ~long:"node-version-check-bypass"
    ~doc:
      "If node-version-check-bypass flag is set, the baker will not check its \
       compatibility with the version of the node to which it is connected."
    ()

let node_version_allowed_arg =
  Tezos_clic.arg
    ~long:"node-version-allowed"
    ~placeholder:"<product>-[v]<major>.<minor>[.0]<info>[:<commit>]"
    ~doc:
      "When specified the baker will accept to run with a node of this \
       version. The specified version is composed of the product, for example \
       'octez'; the major and the minor versions that are positive integers; \
       the info, for example '-rc', '-beta1+dev' or realese if none is \
       provided; optionally the commit that is the hash of the last git commit \
       or a prefix of at least 8 characters long."
    string_parameter

let get_delegates (cctxt : Protocol_client_context.full)
    (pkhs : Signature.public_key_hash list) =
  let open Lwt_result_syntax in
  let proj_delegate (alias, public_key_hash, public_key, secret_key_uri) =
    Baking_state_types.Key.make
      ~alias
      ~public_key_hash
      ~public_key
      ~secret_key_uri
  in
  let* delegates =
    if pkhs = [] then
      let* keys = Client_keys.get_keys cctxt in
      List.map proj_delegate keys |> return
    else
      List.map_es
        (fun pkh ->
          let* result = Client_keys.get_key cctxt pkh in
          match result with
          | alias, pk, sk_uri -> return (proj_delegate (alias, pkh, pk, sk_uri)))
        pkhs
  in
  let* () =
    Tezos_signer_backends.Encrypted.decrypt_list
      cctxt
      (List.map
         (function {Baking_state_types.Key.alias; _} -> alias)
         delegates)
  in
  let delegates_no_duplicates = List.sort_uniq compare delegates in
  let*! () =
    if List.compare_lengths delegates delegates_no_duplicates <> 0 then
      cctxt#warning
        "Warning: the list of public key hash aliases contains duplicate \
         hashes, which are ignored"
    else Lwt.return_unit
  in
  return delegates_no_duplicates

let sources_param =
  Tezos_clic.seq_of_param
    (Client_keys.Public_key_hash.source_param
       ~name:"baker"
       ~desc:
         "name of the delegate owning the attestation/baking right or name of \
          the consensus key signing on the delegate's behalf")

let dal_node_endpoint_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"dal-node"
    ~placeholder:"uri"
    ~doc:"endpoint of the DAL node, e.g. 'http://localhost:8933'"
    (Tezos_clic.parameter (fun _ s -> return @@ Uri.of_string s))

let without_dal_arg =
  Tezos_clic.switch
    ~long:"without-dal"
    ~doc:
      "If without-dal flag is set, the daemon will not try to connect to a DAL \
       node."
    ()

let block_count_arg =
  Tezos_clic.default_arg
    ~long:"count"
    ~short:'n'
    ~placeholder:"block count"
    ~doc:"number of blocks to bake"
    ~default:"1"
  @@ Client_proto_args.positive_int_parameter ()

let create_dal_node_rpc_ctxt endpoint =
  let open Tezos_rpc_http_client_unix in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  let media_types =
    Tezos_rpc_http.Media_type.Command_line.of_command_line rpc_config.media_type
  in
  new RPC_client_unix.http_ctxt rpc_config media_types

let delegate_commands () : Protocol_client_context.full Tezos_clic.command list
    =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  let group =
    {name = "delegate.client"; title = "Tenderbake client commands"}
  in
  [
    command
      ~group
      ~desc:"Benchmark the proof of work challenge resolution"
      (args2
         (default_arg
            ~doc:"Proof of work threshold"
            ~long:"threshold"
            ~placeholder:"int"
            ~default:
              (Int64.to_string
                 Default_parameters.constants_mainnet.proof_of_work_threshold)
            (parameter (fun (cctxt : Protocol_client_context.full) x ->
                 try return (Int64.of_string x)
                 with _ -> cctxt#error "Expect an integer")))
         (arg
            ~doc:"Random seed"
            ~long:"seed"
            ~placeholder:"int"
            (parameter (fun (cctxt : Protocol_client_context.full) x ->
                 try return (int_of_string x)
                 with _ -> cctxt#error "Expect an integer"))))
      (prefix "bench"
      @@ param
           ~name:"nb_draw"
           ~desc:"number of draws"
           (parameter (fun (cctxt : Protocol_client_context.full) x ->
                match int_of_string x with
                | x when x >= 1 -> return x
                | _ | (exception _) ->
                    cctxt#error "Expect a strictly positive integer"))
      @@ fixed ["baking"; "PoW"; "challenges"])
      (fun (proof_of_work_threshold, seed) nb_draw cctxt ->
        let open Lwt_result_syntax in
        let*! () =
          cctxt#message
            "Running %d iterations of proof-of-work challenge..."
            nb_draw
        in
        let rstate =
          match seed with
          | None -> Random.State.make_self_init ()
          | Some s -> Random.State.make [|s|]
        in
        let* all =
          List.map_es
            (fun i ->
              let level = Int32.of_int (Random.State.int rstate (1 lsl 29)) in
              let shell_header =
                Tezos_base.Block_header.
                  {
                    level;
                    proto_level = 1;
                    (* uint8 *)
                    predecessor = Tezos_crypto.Hashed.Block_hash.zero;
                    timestamp = Time.Protocol.epoch;
                    validation_passes = 3;
                    (* uint8 *)
                    operations_hash =
                      Tezos_crypto.Hashed.Operation_list_list_hash.zero;
                    fitness = [];
                    context = Tezos_crypto.Hashed.Context_hash.zero;
                  }
              in
              let now = Time.System.now () in
              let* _ =
                Baking_pow.mine
                  ~proof_of_work_threshold
                  shell_header
                  (fun proof_of_work_nonce ->
                    Protocol.Alpha_context.
                      {
                        Block_header.payload_hash =
                          Protocol.Block_payload_hash.zero;
                        payload_round = Round.zero;
                        seed_nonce_hash = None;
                        proof_of_work_nonce;
                        per_block_votes =
                          {
                            liquidity_baking_vote = Per_block_vote_pass;
                            adaptive_issuance_vote = Per_block_vote_pass;
                          };
                      })
              in
              let _then = Time.System.now () in
              let x = Ptime.diff _then now in
              let*! () = cctxt#message "%d/%d: %a" i nb_draw Ptime.Span.pp x in
              return x)
            (1 -- nb_draw)
        in
        let sum = List.fold_left Ptime.Span.add Ptime.Span.zero all in
        let base, tail = Stdlib.List.(hd all, tl all) in
        let max =
          List.fold_left
            (fun x y -> if Ptime.Span.compare x y > 0 then x else y)
            base
            tail
        in
        let min =
          List.fold_left
            (fun x y -> if Ptime.Span.compare x y <= 0 then x else y)
            base
            tail
        in
        let div = Ptime.Span.to_float_s sum /. float (List.length all) in
        let*! () =
          cctxt#message
            "%d runs: min: %a, max: %a, average: %a"
            nb_draw
            Ptime.Span.pp
            min
            Ptime.Span.pp
            max
            (Format.pp_print_option Ptime.Span.pp)
            (Ptime.Span.of_float_s div)
        in
        return_unit);
    command
      ~group
      ~desc:"Forge and inject block using the delegates' rights."
      (args13
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_apply_from_round_arg
         force_switch
         operations_arg
         data_dir_path_arg
         adaptive_issuance_vote_arg
         do_not_monitor_node_mempool_arg
         dal_node_endpoint_arg
         block_count_arg
         state_recorder_switch_arg)
      (prefixes ["bake"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force_apply_from_round,
             force,
             extra_operations,
             data_dir,
             adaptive_issuance_vote,
             do_not_monitor_node_mempool,
             dal_node_endpoint,
             block_count,
             state_recorder )
           pkhs
           cctxt ->
        let* delegates = get_delegates cctxt pkhs in
        let dal_node_rpc_ctxt =
          Option.map create_dal_node_rpc_ctxt dal_node_endpoint
        in
        Baking_lib.bake
          cctxt
          ?dal_node_rpc_ctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ?force_apply_from_round
          ~force
          ~monitor_node_mempool:(not do_not_monitor_node_mempool)
          ?extra_operations
          ?data_dir
          ~count:block_count
          ?votes:
            (Option.map
               (fun adaptive_issuance_vote ->
                 {
                   Baking_configuration.default_votes_config with
                   adaptive_issuance_vote;
                 })
               adaptive_issuance_vote)
          ~state_recorder
          delegates);
    command
      ~group
      ~desc:"Forge and inject an attestation operation."
      (args1 attestation_force_switch_arg)
      (prefixes ["attest"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        let* delegates = get_delegates cctxt pkhs in
        Baking_lib.attest ~force cctxt delegates);
    command
      ~group
      ~desc:"Forge and inject a preattestation operation."
      (args1 attestation_force_switch_arg)
      (prefixes ["preattest"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        let* delegates = get_delegates cctxt pkhs in
        Baking_lib.preattest ~force cctxt delegates);
    command
      ~group
      ~desc:"Send a Tenderbake proposal"
      (args9
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_apply_from_round_arg
         force_switch
         operations_arg
         data_dir_path_arg
         state_recorder_switch_arg)
      (prefixes ["propose"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force_apply_from_round,
             force,
             extra_operations,
             data_dir,
             state_recorder )
           sources
           cctxt ->
        let* delegates = get_delegates cctxt sources in
        Baking_lib.propose
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ?force_apply_from_round
          ~force
          ?extra_operations
          ?data_dir
          ~state_recorder
          delegates);
    command
      ~group
      ~desc:"Send a block proposal for the current level"
      (args4
         force_switch
         force_round_arg
         minimal_timestamp_switch
         force_reproposal)
      (prefixes ["repropose"; "for"] @@ sources_param)
      (fun (force, force_round, minimal_timestamp, force_reproposal)
           sources
           cctxt ->
        let*? force_round =
          Option.map_e
            (fun r ->
              Result.map_error Environment.wrap_tztrace
              @@ Protocol.Alpha_context.Round.of_int r)
            force_round
        in
        let* delegates = get_delegates cctxt sources in
        Baking_lib.repropose
          cctxt
          ~force
          ~minimal_timestamp
          ~force_reproposal
          ?force_round
          delegates);
  ]

let directory_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ p ->
      let*! exists = Lwt_utils_unix.dir_exists p in
      if not exists then failwith "Directory doesn't exist: '%s'" p
      else return p)

let per_block_vote_file_arg =
  Tezos_clic.arg
    ~doc:"read per block votes as json file"
    ~short:'V'
    ~long:"votefile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun (_cctxt : Protocol_client_context.full) file ->
         let open Lwt_result_syntax in
         let* file_exists =
           protect
             ~on_error:(fun _ -> tzfail (Block_vote_file_not_found file))
             (fun () ->
               let*! b = Lwt_unix.file_exists file in
               return b)
         in
         if file_exists then return file
         else tzfail (Block_vote_file_not_found file)))

let pre_emptive_forge_time_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"pre-emptive-forge-time"
    ~placeholder:"seconds"
    ~doc:
      "Sets the pre-emptive forge time optimization, in seconds. When set, the \
       baker, if it is the next level round 0 proposer, will start forging \
       after quorum has been reached in the current level while idly waiting \
       for it to end. When it is its time to propose, the baker will inject \
       the pre-emptively forged block immediately, allowing more time for the \
       network to reach quorum on it. Operators should note that the higher \
       this value `t`, the lower the operation inclusion window (specifically \
       `block_time - t`) which may lead to lower baking rewards. Defaults to \
       15/% of block time. Set to 0 to ignore pre-emptive forging."
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s)
         with _ -> failwith "pre-emptive-forge-time expected int or float."))

let remote_calls_timeout_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"remote-calls-timeout"
    ~placeholder:"seconds"
    ~doc:
      "Sets a timeout for client calls such as signing block header or \
       attestation and for the creation of deterministic nonce. Use only if \
       your remote signer can handle concurrent requests."
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s)
         with _ -> failwith "remote-calls-timeout expected int or float."))

type baking_mode = Local of {local_data_dir_path : string} | Remote

let baker_args =
  Tezos_clic.args17
    pidfile_arg
    node_version_check_bypass_arg
    node_version_allowed_arg
    minimal_fees_arg
    minimal_nanotez_per_gas_unit_arg
    minimal_nanotez_per_byte_arg
    force_apply_from_round_arg
    keep_alive_arg
    liquidity_baking_toggle_vote_arg
    adaptive_issuance_vote_arg
    per_block_vote_file_arg
    operations_arg
    dal_node_endpoint_arg
    without_dal_arg
    state_recorder_switch_arg
    pre_emptive_forge_time_arg
    remote_calls_timeout_arg

(* /*\ DO NOT MODIFY /!\

   If you do, you need to have the command in sync with the agnostic baker one. This command
   is meant to removed, the source of truth is the agnostic baker.
*)
let run_baker ?(recommend_agnostic_baker = true)
    ( pidfile,
      node_version_check_bypass,
      node_version_allowed,
      minimal_fees,
      minimal_nanotez_per_gas_unit,
      minimal_nanotez_per_byte,
      force_apply_from_round,
      keep_alive,
      liquidity_baking_vote,
      adaptive_issuance_vote,
      per_block_vote_file,
      extra_operations,
      dal_node_endpoint,
      without_dal,
      state_recorder,
      pre_emptive_forge_time,
      remote_calls_timeout ) baking_mode sources cctxt =
  let open Lwt_result_syntax in
  Command_run.may_lock_pidfile pidfile @@ fun () ->
  let*! () =
    if recommend_agnostic_baker then Events.(emit recommend_octez_baker ())
    else Lwt.return_unit
  in
  let* () =
    Octez_agnostic_baker.Command_run.check_node_version
      cctxt
      node_version_check_bypass
      node_version_allowed
  in
  let*! per_block_vote_file =
    if per_block_vote_file = None then
      (* If the votes file was not explicitly given, we
         look into default locations. *)
      Octez_agnostic_baker.Per_block_vote_file.lookup_default_vote_file_path
        (cctxt :> Client_context.full)
    else Lwt.return per_block_vote_file
  in
  let*! () =
    if Option.is_some adaptive_issuance_vote then
      Events.(emit unused_cli_adaptive_issuance_vote ())
    else Lwt.return_unit
  in
  (* We don't let the user run the baker without providing some
     option (CLI, file path, or file in default location) for
     the per-block votes. *)
  let* votes =
    let of_protocol = function
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_on ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_on
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_off ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_off
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_pass
    in
    let to_protocol = function
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_on ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_off ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_off
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_pass ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass
    in
    let* Octez_agnostic_baker.Configuration.
           {vote_file; liquidity_baking_vote; adaptive_issuance_vote} =
      Octez_agnostic_baker.Per_block_vote_file.load_per_block_votes_config
        ~default_liquidity_baking_vote:
          (Option.map of_protocol liquidity_baking_vote)
        ~per_block_vote_file
    in
    return
      Baking_configuration.
        {
          vote_file;
          liquidity_baking_vote = to_protocol liquidity_baking_vote;
          adaptive_issuance_vote = to_protocol adaptive_issuance_vote;
        }
  in
  let dal_node_rpc_ctxt =
    Option.map create_dal_node_rpc_ctxt dal_node_endpoint
  in
  let* () = Command_run.check_dal_node without_dal dal_node_rpc_ctxt in
  let* delegates = get_delegates cctxt sources in
  let data_dir =
    match baking_mode with
    | Local {local_data_dir_path} -> Some local_data_dir_path
    | Remote -> None
  in
  Client_daemon.Baker.run
    cctxt
    ?dal_node_rpc_ctxt
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    ~votes
    ?extra_operations
    ?pre_emptive_forge_time
    ?force_apply_from_round
    ?remote_calls_timeout
    ~chain:cctxt#chain
    ?data_dir
    ~keep_alive
    ~state_recorder
    delegates

(* /*\ DO NOT MODIFY /!\

   If you do, you need to have the command in sync with the agnostic baker one. This command
   is meant to removed, the source of truth is the agnostic baker.
*)
let baker_commands () : Protocol_client_context.full Tezos_clic.command list =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      baker_args
      (prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"node_data_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ sources_param)
      (fun args local_data_dir_path sources cctxt ->
        let baking_mode = Local {local_data_dir_path} in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let baking_mode = Remote in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun (pidfile, keep_alive) cctxt ->
        Command_run.may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive);
  ]

(* /*\ DO NOT MODIFY /!\

   If you do, you need to have the command in sync with the agnostic baker one. This command
   is meant to removed, the source of truth is the agnostic baker.
*)
let accuser_commands () =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.accuser";
      title = "Commands related to the accuser daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg Client_proto_args.preserved_levels_arg keep_alive_arg)
      (prefixes ["run"] @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        Command_run.may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.Accuser.run
          cctxt
          ~chain:cctxt#chain
          ~preserved_levels
          ~keep_alive);
  ]
