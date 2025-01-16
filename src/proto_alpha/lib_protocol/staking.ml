(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error +=
  | Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate

let () =
  let description =
    "A contract tries to stake to its delegate while having unstake requests \
     to a previous delegate that cannot be finalized yet. Try again in a later \
     cycle (no more than consensus_rights_delay + max_slashing_period)."
  in
  register_error_kind
    `Temporary
    ~id:
      "operation.cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate"
    ~title:
      "Cannot stake with unfinalizable unstake requests to another delegate"
    ~description
    Data_encoding.unit
    (function
      | Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate ->
          Some ()
      | _ -> None)
    (fun () ->
      Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate)

let perform_finalizable_unstake_transfers ctxt contract finalizable =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (ctxt, balance_updates) (delegate, cycle, amount) ->
      let+ ctxt, new_balance_updates =
        Token.transfer
          ctxt
          (`Unstaked_frozen_deposits
            (Unstaked_frozen_staker_repr.Single (contract, delegate), cycle))
          (`Contract contract)
          amount
      in
      (ctxt, new_balance_updates @ balance_updates))
    (ctxt, [])
    finalizable

let finalize_unstake ctxt contract =
  let open Lwt_result_syntax in
  let check_unfinalizable ctxt _unfinalizable = return ctxt in
  let* ctxt, balance_updates, _ =
    Unstake_requests_storage.finalize_unstake_and_check
      ~check_unfinalizable
      ~perform_finalizable_unstake_transfers
      ctxt
      contract
  in
  return (ctxt, balance_updates)

let can_stake_from_unstake ctxt ~delegate =
  let open Lwt_result_syntax in
  let* slashing_history_opt = Storage.Slashed_deposits.find ctxt delegate in
  let slashing_history = Option.value slashing_history_opt ~default:[] in

  let* slashing_history_opt_o =
    Storage.Contract.Slashed_deposits__Oxford.find
      ctxt
      (Contract_repr.Implicit delegate)
  in
  let slashing_history_o =
    Option.value slashing_history_opt_o ~default:[]
    |> List.map (fun (a, b) -> (a, Percentage.convert_from_o_to_p b))
  in

  let slashing_history = slashing_history @ slashing_history_o in

  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  let oldest_slashable_cycle =
    Cycle_repr.sub current_cycle (slashable_deposits_period + 1)
    |> Option.value ~default:Cycle_repr.root
  in
  let*! is_denounced =
    Pending_denunciations_storage.has_pending_denunciations ctxt delegate
  in
  let is_slashed =
    List.exists
      (fun (x, _) -> Cycle_repr.(x >= oldest_slashable_cycle))
      slashing_history
  in
  return @@ not (is_denounced || is_slashed)

let stake_from_unstake_for_delegate ctxt ~delegate ~unfinalizable_requests_opt
    amount =
  let open Lwt_result_syntax in
  let remove_from_unstaked_frozen_deposit ctxt cycle delegate sender_contract
      amount =
    let* ctxt, balance_updates =
      Token.transfer
        ctxt
        (`Unstaked_frozen_deposits
          (Unstaked_frozen_staker_repr.Single (sender_contract, delegate), cycle))
        (`Frozen_deposits
          (Frozen_staker_repr.single_staker ~staker:sender_contract ~delegate))
        amount
    in
    let* ctxt =
      Unstaked_frozen_deposits_storage
      .decrease_initial_amount_only_for_stake_from_unstake
        ctxt
        delegate
        cycle
        amount
    in
    return (ctxt, balance_updates)
  in
  match unfinalizable_requests_opt with
  | None -> return (ctxt, [], amount)
  | Some Unstake_requests_storage.{delegate = delegate_requests; requests} ->
      if
        Signature.Public_key_hash.(delegate <> delegate_requests)
        && not (List.is_empty requests)
      then (* Should not be possible *)
        return (ctxt, [], Tez_repr.zero)
      else
        let* allowed = can_stake_from_unstake ctxt ~delegate in
        if not allowed then
          (* a slash could have modified the unstaked frozen deposits: cannot stake from unstake *)
          return (ctxt, [], amount)
        else
          let sender_contract = Contract_repr.Implicit delegate in
          let requests_sorted =
            List.sort
              (fun (cycle1, _) (cycle2, _) ->
                Cycle_repr.compare cycle2 cycle1
                (* decreasing cycle order, to release first the tokens
                   that would be frozen for the longest time *))
              requests
          in
          let rec transfer_from_unstake ctxt balance_updates
              remaining_amount_to_transfer updated_requests_rev requests =
            if Tez_repr.(remaining_amount_to_transfer = zero) then
              return
                ( ctxt,
                  balance_updates,
                  Tez_repr.zero,
                  List.rev_append requests updated_requests_rev )
            else
              match requests with
              | [] ->
                  return
                    ( ctxt,
                      balance_updates,
                      remaining_amount_to_transfer,
                      updated_requests_rev )
              | (cycle, requested_amount) :: t ->
                  if Tez_repr.(remaining_amount_to_transfer >= requested_amount)
                  then
                    let* ctxt, cycle_balance_updates =
                      remove_from_unstaked_frozen_deposit
                        ctxt
                        cycle
                        delegate
                        sender_contract
                        requested_amount
                    in
                    let*? remaining_amount =
                      Tez_repr.(
                        remaining_amount_to_transfer -? requested_amount)
                    in
                    transfer_from_unstake
                      ctxt
                      (balance_updates @ cycle_balance_updates)
                      remaining_amount
                      updated_requests_rev
                      t
                  else
                    let* ctxt, cycle_balance_updates =
                      remove_from_unstaked_frozen_deposit
                        ctxt
                        cycle
                        delegate
                        sender_contract
                        remaining_amount_to_transfer
                    in
                    let*? new_requested_amount =
                      Tez_repr.(
                        requested_amount -? remaining_amount_to_transfer)
                    in
                    return
                      ( ctxt,
                        balance_updates @ cycle_balance_updates,
                        Tez_repr.zero,
                        List.rev_append
                          t
                          ((cycle, new_requested_amount) :: updated_requests_rev)
                      )
          in
          let* ( ctxt,
                 balance_updates,
                 remaining_amount_to_transfer,
                 updated_requests_rev ) =
            transfer_from_unstake ctxt [] amount [] requests_sorted
          in
          let updated_requests = List.rev updated_requests_rev in
          let* ctxt =
            Unstake_requests_storage.update
              ctxt
              sender_contract
              {delegate; requests = updated_requests}
          in
          return (ctxt, balance_updates, remaining_amount_to_transfer)

let stake ctxt ~(amount : Tez_repr.t) ~sender ~delegate =
  let open Lwt_result_syntax in
  let check_unfinalizable ctxt
      Unstake_requests_storage.{delegate = unstake_delegate; requests} =
    match requests with
    | [] -> return ctxt
    | _ :: _ ->
        if Signature.Public_key_hash.(delegate <> unstake_delegate) then
          tzfail
            Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate
        else return ctxt
  in
  let sender_contract = Contract_repr.Implicit sender in
  let* ctxt, finalize_balance_updates, unfinalizable_requests_opt =
    Unstake_requests_storage.finalize_unstake_and_check
      ~check_unfinalizable
      ~perform_finalizable_unstake_transfers
      ctxt
      sender_contract
  in
  (* stake from unstake for eligible delegates *)
  let* ctxt, stake_balance_updates1, amount_from_liquid =
    if Signature.Public_key_hash.(sender <> delegate) then
      return (ctxt, [], amount)
    else
      stake_from_unstake_for_delegate
        ctxt
        ~delegate
        ~unfinalizable_requests_opt
        amount
  in
  (* Issue pseudotokens for delegators *)
  let* ctxt, stake_balance_updates2 =
    if Signature.Public_key_hash.(sender <> delegate) then
      Staking_pseudotokens_storage.stake
        ctxt
        ~contract:sender_contract
        ~delegate
        amount_from_liquid
    else return (ctxt, [])
  in
  let+ ctxt, stake_balance_updates3 =
    Token.transfer
      ctxt
      (`Contract sender_contract)
      (`Frozen_deposits
        (Frozen_staker_repr.single_staker ~staker:sender_contract ~delegate))
      amount_from_liquid
  in
  ( ctxt,
    stake_balance_updates1 @ stake_balance_updates2 @ stake_balance_updates3
    @ finalize_balance_updates )

let request_unstake ctxt ~sender_contract ~delegate requested_amount =
  let open Lwt_result_syntax in
  let* ctxt, tez_to_unstake, request_unstake_balance_updates =
    Staking_pseudotokens_storage.request_unstake
      ctxt
      ~contract:sender_contract
      ~delegate
      requested_amount
  in
  if Tez_repr.(tez_to_unstake = zero) then
    return (ctxt, request_unstake_balance_updates)
  else
    let*? ctxt =
      Raw_context.consume_gas ctxt Adaptive_issuance_costs.request_unstake_cost
    in
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let* ctxt, balance_updates =
      Token.transfer
        ctxt
        (`Frozen_deposits
          (Frozen_staker_repr.single_staker ~staker:sender_contract ~delegate))
        (`Unstaked_frozen_deposits
          ( Unstaked_frozen_staker_repr.Single (sender_contract, delegate),
            current_cycle ))
        tez_to_unstake
    in
    let* ctxt, finalize_balance_updates =
      finalize_unstake ctxt sender_contract
    in
    let+ ctxt =
      Unstake_requests_storage.add
        ctxt
        ~contract:sender_contract
        ~delegate
        current_cycle
        tez_to_unstake
    in
    ( ctxt,
      request_unstake_balance_updates @ balance_updates
      @ finalize_balance_updates )
