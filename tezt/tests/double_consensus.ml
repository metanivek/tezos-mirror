(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Accuser
   Invocation:   dune exec tezt/tests/main.exe -- --file double_consensus.ml
   Subject:      Detect double (pre)attestation through the accuser.
*)

let team = Tag.layer1

let bootstrap1, bootstrap2, bootstrap3, bootstrap4, bootstrap5 =
  Constant.(bootstrap1, bootstrap2, bootstrap3, bootstrap4, bootstrap5)

let public_key_hashes =
  List.map (fun (account : Account.key) -> account.public_key_hash)

let delegate_forbidden_error =
  rex
    "has committed too many misbehaviours; it is temporarily not allowed to \
     bake/preattest/attest."

let double_attestation_waiter accuser =
  Accuser.wait_for accuser (sf "double_attestation_denounced.v0") (fun _ ->
      Some ())

let double_preattestation_waiter accuser =
  Accuser.wait_for accuser (sf "double_preattestation_denounced.v0") (fun _ ->
      Some ())

let double_consensus_already_denounced_waiter accuser oph =
  Accuser.wait_for accuser "double_consensus_already_denounced.v0" (fun json ->
      if String.equal (JSON.as_string json) oph then Some () else None)

let get_double_consensus_denounciation_hash protocol consensus_name client =
  let double_consensus_kind =
    if Protocol.(number protocol > number R022) then
      "double_consensus_operation_evidence"
    else sf "double_%s_evidence" consensus_name
  in
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let ops = JSON.(mempool |-> "validated" |> as_list) in
  let op =
    List.find_map
      (fun op ->
        let kind =
          JSON.(op |-> "contents" |> as_list |> List.hd |-> "kind" |> as_string)
        in
        if String.equal kind double_consensus_kind then
          Some JSON.(op |-> "hash" |> as_string)
        else None)
      ops
  in
  match op with
  | None -> failwith "Denunciation not found in the mempool"
  | Some op -> return op

let double_attestation_init
    (consensus_for :
      ?endpoint:Client.endpoint ->
      ?protocol:Protocol.t ->
      ?key:string list ->
      ?force:bool ->
      Client.t ->
      unit Lwt.t) ?parameter_file consensus_name protocol () =
  let* node, client =
    Client.init_with_protocol ?parameter_file ~protocol `Client ()
  in
  let* accuser = Accuser.init ~event_level:`Debug ~protocol node in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Recover available slots for %s." Constant.bootstrap1.alias ;
  let* slots =
    Client.RPC.call client
    @@ RPC.get_chain_block_helper_validators
         ~delegate:Constant.bootstrap1.public_key_hash
         ()
  in
  let slots =
    List.map
      JSON.as_int
      JSON.(List.hd JSON.(slots |> as_list) |-> "slots" |> as_list)
  in
  Log.info "Inject valid %s." consensus_name ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    consensus_for ~protocol ~force:true ~key:[Constant.bootstrap1.alias] client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let level = JSON.(content |-> "level" |> as_int) in
  let round = JSON.(content |-> "round" |> as_int) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  return ((client, accuser), (branch, level, round, slots, block_payload_hash))

let attest_utils =
  ( Client.attest_for,
    (fun ~slot ~level ~round ~block_payload_hash ->
      Operation.Consensus.attestation ~slot ~level ~round ~block_payload_hash ()),
    double_attestation_waiter,
    "attestation" )

let preattest_utils =
  ( Client.preattest_for,
    Operation.Consensus.preattestation,
    double_preattestation_waiter,
    "preattestation" )

let double_consensus_wrong_block_payload_hash
    (consensus_for, mk_consensus, consensus_waiter, consensus_name) protocol =
  let* (client, accuser), (branch, level, round, slots, _block_payload_hash) =
    double_attestation_init consensus_for consensus_name protocol ()
  in
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block:"head~2" ()
  in
  let block_payload_hash = JSON.(header |-> "payload_hash" |> as_string) in
  Log.info "Inject an invalid %s and wait for denounciation" consensus_name ;
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let waiter = consensus_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  Log.info
    "Inject another invalid %s and wait for already_denounced event"
    consensus_name ;
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block:"head~3" ()
  in
  let block_payload_hash = JSON.(header |-> "payload_hash" |> as_string) in
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let* oph =
    get_double_consensus_denounciation_hash protocol consensus_name client
  in
  let waiter_already_denounced =
    double_consensus_already_denounced_waiter accuser oph
  in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter_already_denounced in
  unit

let double_attestation_wrong_block_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"double attestation using wrong block_payload_hash"
    ~tags:
      [
        Tag.layer1;
        "double";
        "attestation";
        "accuser";
        "block_payload_hash";
        "node";
      ]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  double_consensus_wrong_block_payload_hash attest_utils protocol

let double_preattestation_wrong_block_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"double preattestation using wrong block_payload_hash"
    ~tags:
      [
        Tag.layer1;
        "double";
        "preattestation";
        "accuser";
        "block_payload_hash";
        "node";
      ]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  double_consensus_wrong_block_payload_hash preattest_utils protocol

let double_consensus_wrong_branch
    (consensus_for, mk_consensus, consensus_waiter, consensus_name) protocol =
  let* (client, accuser), (_branch, level, round, slots, block_payload_hash) =
    double_attestation_init consensus_for consensus_name protocol ()
  in
  let* branch = Operation.Manager.get_branch ~offset:4 client in
  Log.info "Inject an invalid %s and wait for denounciation" consensus_name ;
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let waiter = consensus_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  Log.info
    "Inject another invalid %s and wait for already_denounced event"
    consensus_name ;
  let* branch = Operation.Manager.get_branch ~offset:5 client in
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let* oph =
    get_double_consensus_denounciation_hash protocol consensus_name client
  in
  let waiter_already_denounced =
    double_consensus_already_denounced_waiter accuser oph
  in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter_already_denounced in
  unit

let double_attestation_wrong_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"double attestation using wrong branch"
    ~tags:[Tag.layer1; "double"; "attestation"; "accuser"; "branch"; "node"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol -> double_consensus_wrong_branch attest_utils protocol

let double_preattestation_wrong_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"double preattestation using wrong branch"
    ~tags:[Tag.layer1; "double"; "preattestation"; "accuser"; "branch"; "node"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol -> double_consensus_wrong_branch preattest_utils protocol

let consensus_operation_too_old_waiter accuser =
  Accuser.wait_for accuser "consensus_operation_too_old.v0" (fun _ -> Some ())

let operation_too_old =
  Protocol.register_test
    ~__FILE__
    ~title:"operation too old"
    ~tags:[Tag.layer1; "accuser"; "old"; "operation"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* accuser =
    (* We set the preserved_levels to 0 to ensure that an operation for the
       previous level is accepted by the mempool and discarded by the
       accuser. *)
    Accuser.init ~preserved_levels:0 ~event_level:`Debug ~protocol node
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Inject valid attestation." ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    Client.attest_for
      ~protocol
      ~force:true
      ~key:[Constant.bootstrap1.alias]
      client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let level = JSON.(content |-> "level" |> as_int) in
  let slot = JSON.(content |-> "slot" |> as_int) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  Log.info "Bake 1 block." ;
  let* () = Client.bake_for_and_wait client in
  Log.info
    "Craft and inject an attestation 1 level in the past and wait for \
     [consensus_operation_too_old.v0] event from the accuser." ;
  let op =
    Operation.Consensus.attestation ~slot ~level ~round:3 ~block_payload_hash ()
  in
  let waiter = consensus_operation_too_old_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  unit

let consensus_operation_too_far_in_future_waiter accuser =
  Accuser.wait_for accuser "consensus_operation_too_far_in_future.v0" (fun _ ->
      Some ())

let operation_too_far_in_future =
  Protocol.register_test
    ~__FILE__
    ~title:"operation too far in the future"
    ~tags:[Tag.layer1; "accuser"; "future"; "operation"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* accuser =
    Accuser.init ~preserved_levels:2 ~event_level:`Debug ~protocol node
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Inject valid attestation." ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    Client.attest_for
      ~protocol
      ~force:true
      ~key:[Constant.bootstrap1.alias]
      client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  let level = 6 in
  Log.info
    "Recover available slots for %s at level %d."
    Constant.bootstrap1.alias
    level ;
  let* slots =
    Client.RPC.call client
    @@ RPC.get_chain_block_helper_validators
         ~delegate:Constant.bootstrap1.public_key_hash
         ~level
         ()
  in
  let slots =
    List.map
      JSON.as_int
      JSON.(List.hd JSON.(slots |> as_list) |-> "slots" |> as_list)
  in
  Log.info
    "Craft and inject an attestation 3 levels in the future and wait for \
     [consensus_operation_too_far_in_future.v0] event from the accuser." ;
  let op =
    Operation.Consensus.attestation
      ~slot:(List.hd slots)
      ~level
      ~round:0
      ~block_payload_hash
      ()
  in
  let waiter = consensus_operation_too_far_in_future_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  unit

let fetch_anonymous_operations ?block client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ?block
         ~validation_pass:2
         ()
  in
  return JSON.(as_list json)

type kind =
  | Preattestation
  | Attestation
  | Attestations_aggregate
  | Preattestations_aggregate

let string_of_kind = function
  | Preattestation -> "preattestation"
  | Attestation -> "attestation"
  | Attestations_aggregate -> "attestations_aggregate"
  | Preattestations_aggregate -> "preattestations_aggregate"

(* [filter_consensus_denunciations ~level ~round ~delegate ~evidence ops]
   filters [ops], keeping only "double_consensus_operation_evidence" operations
   that match the given [level], [round], [delegate], and [evidence]. *)
let filter_consensus_denunciations ~level ~round ~delegate ~evidence
    anonymous_operations =
  List.filter
    JSON.(
      fun operation_json ->
        let contents = operation_json |-> "contents" |> as_list |> List.hd in
        let forbidden_delegate =
          contents |-> "metadata" |-> "punished_delegate" |> as_string
        in
        let kind = contents |-> "kind" |> as_string in
        match kind with
        | "double_consensus_operation_evidence"
          when delegate = forbidden_delegate ->
            let kind1, kind2 = evidence in
            let op1 = contents |-> "op1" |-> "operations" in
            let op2 = contents |-> "op2" |-> "operations" in
            let kind1' = op1 |-> "kind" |> as_string in
            let kind2' = op2 |-> "kind" |> as_string in
            let level', round' =
              let consensus_content_opt =
                op1 |-> "consensus_content" |> as_opt
              in
              match consensus_content_opt with
              | Some consensus_content ->
                  (* aggregate operations *)
                  let level = consensus_content |-> "level" |> as_int in
                  let round = consensus_content |-> "round" |> as_int in
                  (level, round)
              | None ->
                  (* non-aggregate operations *)
                  let level = op1 |-> "level" |> as_int in
                  let round = op1 |-> "round" |> as_int in
                  (level, round)
            in
            let kind1 = string_of_kind kind1 in
            let kind2 = string_of_kind kind2 in
            level = level' && round = round'
            && ((kind1 = kind1' && kind2 = kind2')
               || (kind1 = kind2' && kind2 = kind1'))
        | _ -> false)
    anonymous_operations

(* [check_contains_consensus_denunciations ~loc ~level ~round
   ~anonymous_operations denunciations]
   asserts that, for each (delegate, evidence) pair in [denunciations], there
   is exactly one matching "double_consensus_operation_evidence" operation
   in [anonymous_operations] for the given [level] and [round].
   Fails the test otherwise. *)
let check_contains_consensus_denunciations ~loc ~level ~round
    ~anonymous_operations denunciations =
  List.iter
    (fun (delegate, evidence) ->
      let delegate = delegate.Account.public_key_hash in
      let corresponding_denunciations =
        filter_consensus_denunciations
          ~level
          ~round
          ~delegate
          ~evidence
          anonymous_operations
      in
      match corresponding_denunciations with
      | [] ->
          Test.fail
            "%s@.No such denunciation found for %s at level %d round %d"
            loc
            delegate
            level
            round
      | _ :: [] -> ()
      | _ :: _ :: _ ->
          Test.fail
            "%s@.Unexpected multiple corresponding denunciations found"
            loc)
    denunciations

let attestation_and_aggregation_wrong_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"attestation and aggregation wrong payload hash"
    ~tags:[Tag.layer1; "attestation"; "aggregation"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let consensus_rights_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["allow_tz4_delegate_enable"], `Bool true);
        (["aggregate_attestation"], `Bool true);
        (* Diminish some constants to activate consensus keys faster *)
        (["blocks_per_cycle"], `Int 2);
        (["nonce_revelation_threshold"], `Int 1);
        (["consensus_rights_delay"], `Int consensus_rights_delay);
        (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
        (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
      ]
  in
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~parameter_file ()
  in
  let* _ = Node.wait_for_level node 1 in
  (* Run accusers (simulates a network were multiple denunciations are received
     simultaneously) *)
  let* _accuser1 = Accuser.init ~protocol node in
  let* _accuser2 = Accuser.init ~protocol node in
  (* Set a BLS consensus key for some bootstrap accounts *)
  let* ck1 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap1 client in
  let* ck2 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap2 client in
  let* ck3 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap3 client in
  let keys =
    public_key_hashes
      [
        ck1; ck2; ck3; bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5;
      ]
  in
  (* Bake until consensus keys become active *)
  let* level = Client.bake_for_and_wait_level ~keys ~count:6 client in
  (* Bake a block: it is expected to contain an attestations_aggregate with all
     bootstrap accounts *)
  let* () = Client.bake_for_and_wait ~keys client in
  (* Attest for bootstrap1 with a dummy block_payload_hash *)
  let* _ =
    let open Operation.Consensus in
    let* slots = get_slots_by_consensus_key ~level client in
    let slot = first_slot ~slots ck1 in
    let* branch = get_branch ~attested_level:level client in
    let* bph_level_4 = get_block_payload_hash ~block:"4" client in
    attest_for
      ~protocol
      ~branch
      ~slot
      ~level
      ~round:0
      ~block_payload_hash:bph_level_4
      ck1
      client
  in
  (* Bake a block: it is expected to contain a denunciation. *)
  let* level = Client.bake_for_and_wait_level ~keys client in
  (* Fetch anonymous operations from the current head
     and check for the expected denunciation *)
  let* anonymous_operations = fetch_anonymous_operations client in
  let () =
    check_contains_consensus_denunciations
      ~loc:__LOC__
      ~level:(level - 2)
      ~round:0
      ~anonymous_operations
      [(bootstrap1, (Attestation, Attestations_aggregate))]
  in
  let open Operation.Consensus in
  let* slots = get_slots_by_consensus_key ~level client in
  let* branch = get_branch ~attested_level:level client in
  let* block_payload_hash = get_block_payload_hash client in
  (* Attest for bootstrap1 and check that he is forbidden *)
  let* (`OpHash _) =
    let slot = first_slot ~slots ck1 in
    attest_for
      ~error:delegate_forbidden_error
      ~protocol
      ~branch
      ~slot
      ~level
      ~round:0
      ~block_payload_hash
      ck1
      client
  in
  (* Attest for bootstrap2 and expect a success *)
  let* (`OpHash _) =
    let slot = first_slot ~slots ck2 in
    attest_for
      ~protocol
      ~branch
      ~slot
      ~level
      ~round:0
      ~block_payload_hash
      ck2
      client
  in
  unit

let double_aggregation_wrong_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"double aggregation wrong payload hash"
    ~tags:[Tag.layer1; "double"; "aggregation"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let consensus_rights_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["allow_tz4_delegate_enable"], `Bool true);
        (["aggregate_attestation"], `Bool true);
        (* Diminish some constants to activate consensus keys faster. *)
        (["blocks_per_cycle"], `Int 3);
        (["nonce_revelation_threshold"], `Int 1);
        (["consensus_rights_delay"], `Int consensus_rights_delay);
        (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
        (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
      ]
  in
  let* node1, client1 =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      ()
  in
  let* node2, client2 =
    Client.init_with_node
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ()
  in
  let* node1_id = Node.wait_for_identity node1 in
  let* node2_id = Node.wait_for_identity node2 in
  (* Connect nodes together *)
  let* () = Client.Admin.connect_address ~peer:node1 client2 in
  let* () = Client.Admin.connect_address ~peer:node2 client1 in
  let* _ = Node.wait_for_level node2 1 in
  (* Run accusers *)
  let* _accuser1 = Accuser.init ~protocol node1 in
  let* _accuser2 = Accuser.init ~protocol node2 in
  (* Set BLS consensus key for some bootstrap accounts *)
  let* ck1 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap1 client1 in
  let* ck2 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap2 client1 in
  let* ck3 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap3 client1 in
  (* Import the consensus keys in client2 aswell *)
  let* () = Client.import_secret_key ~alias:"ck1" client2 ck1.secret_key in
  let* () = Client.import_secret_key ~alias:"ck2" client2 ck2.secret_key in
  let* () = Client.import_secret_key ~alias:"ck3" client2 ck3.secret_key in
  let keys =
    [
      ck1.public_key_hash;
      ck2.public_key_hash;
      ck3.public_key_hash;
      bootstrap1.public_key_hash;
      bootstrap2.public_key_hash;
      bootstrap3.public_key_hash;
      bootstrap4.public_key_hash;
      bootstrap5.public_key_hash;
    ]
  in
  let double_attesting_keys =
    [ck1.public_key_hash; ck2.public_key_hash; bootstrap4.public_key_hash]
  in
  (* Bake until consensus keys become active *)
  let* level = Client.bake_for_and_wait_level ~keys ~count:5 client1 in
  let* _ = Node.wait_for_level node2 level in
  (* Disconnect nodes *)
  let* () = Client.Admin.kick_peer ~peer:node2_id client1 in
  let* () = Client.Admin.kick_peer ~peer:node1_id client2 in
  (* On node1's branch, bake 2 blocks *)
  let* () = Client.bake_for_and_wait ~count:2 ~keys client1 in
  (* On node2's branch, inject a transfer and bake 3 blocks *)
  let* _ = Operation.Manager.inject_single_transfer client2 in
  let* level =
    Client.bake_for_and_wait_level ~count:3 ~keys:double_attesting_keys client2
  in
  (* Reconnect nodes together *)
  let* () = Client.Admin.connect_address ~peer:node1 client2 in
  let* () = Client.Admin.connect_address ~peer:node2 client1 in
  (* node1 is expected to switch to node2's branch *)
  let* _ = Node.wait_for_level node1 level in
  (* Bake a block: it is expected to contain denunciations *)
  let* level = Client.bake_for_and_wait_level ~keys client1 in
  (* Fetch anonymous operations from the current head
     and check for the expected denunciations *)
  let* anonymous_operations = fetch_anonymous_operations client1 in
  let () =
    check_contains_consensus_denunciations
      ~loc:__LOC__
      ~level:(level - 3)
      ~round:0
      ~anonymous_operations
      [
        (bootstrap1, (Attestations_aggregate, Attestations_aggregate));
        (bootstrap2, (Attestations_aggregate, Attestations_aggregate));
        (bootstrap4, (Attestation, Attestation));
      ]
  in
  let open Operation.Consensus in
  let* branch = get_branch ~attested_level:level client1 in
  let* block_payload_hash = get_block_payload_hash client1 in
  let* slots = get_slots_by_consensus_key ~level client1 in
  (* Attest with the double attesting keys and check that they are forbidden *)
  let* () =
    Lwt_list.iter_s
      (fun ck ->
        let slot = first_slot ~slots ck in
        let* (`OpHash _) =
          attest_for
            ~error:delegate_forbidden_error
            ~protocol
            ~branch
            ~slot
            ~level
            ~round:0
            ~block_payload_hash
            ck
            client1
        in
        unit)
      [ck1; ck2; bootstrap4]
  in
  (* Attest for the other keys and expect a success *)
  let* () =
    Lwt_list.iter_s
      (fun ck ->
        let slot = first_slot ~slots ck in
        let* (`OpHash _) =
          attest_for
            ~protocol
            ~branch
            ~slot
            ~level
            ~round:0
            ~block_payload_hash
            ck
            client1
        in
        unit)
      [ck3; bootstrap5]
  in
  unit

let register ~protocols =
  double_attestation_wrong_block_payload_hash protocols ;
  double_preattestation_wrong_block_payload_hash protocols ;
  double_attestation_wrong_branch protocols ;
  double_preattestation_wrong_branch protocols ;
  operation_too_old protocols ;
  operation_too_far_in_future protocols ;
  attestation_and_aggregation_wrong_payload_hash protocols ;
  double_aggregation_wrong_payload_hash protocols
