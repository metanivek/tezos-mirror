(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Protocol_client_context
open Alpha_context

type error += Contract_without_code of Contract.t

let () =
  register_error_kind
    `Permanent
    ~id:"contractWithoutCode"
    ~title:"The given contract has no code"
    ~description:
      "Attempt to get the code of a contract failed because it has nocode. No \
       scriptless contract should remain."
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract has no code %a." Contract.pp contract)
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Contract_without_code c -> Some c | _ -> None)
    (fun c -> Contract_without_code c)

let print_errors (cctxt : #Client_context.printer) trace =
  cctxt#error "%a" Error_monad.pp_print_trace trace >>= fun () -> return_unit

let script_entrypoint_type cctxt ~(chain : Chain_services.chain) ~block
    (program : Script.expr) ~entrypoint =
  Plugin.RPC.Scripts.entrypoint_type
    cctxt
    (chain, block)
    ~script:program
    ~entrypoint
  >>= function
  | Ok ty -> return_some ty
  | Error
      (Environment.Ecoproto_error (Script_tc_errors.No_such_entrypoint _) :: _)
    ->
      return None
  | Error _ as err -> Lwt.return err

let contract_entrypoint_type cctxt ~(chain : Chain_services.chain) ~block
    ~contract ~entrypoint =
  Alpha_services.Contract.entrypoint_type
    cctxt
    (chain, block)
    contract
    entrypoint
  >>= function
  | Ok ty -> return_some ty
  | Error (Tezos_rpc.Context.Not_found _ :: _) -> return None
  | Error _ as err -> Lwt.return err

let print_entrypoint_type (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name ~entrypoint
    = function
  | Ok (Some ty) ->
      (if emacs then
         cctxt#message
           "@[<v 2>((entrypoint . %s) (type . %a))@]@."
           entrypoint
           Michelson_v1_emacs.print_expr
           ty
       else
         cctxt#message
           "@[<v 2>Entrypoint %s: %a@]@."
           entrypoint
           Michelson_v1_printer.print_expr
           ty)
      >>= fun () -> return_unit
  | Ok None ->
      cctxt#message
        "@[<v 2>No entrypoint named %s%a%a@]@."
        entrypoint
        (Format.pp_print_option (fun ppf ->
             Format.fprintf ppf " for contract %a" Contract.pp))
        contract
        (Format.pp_print_option (fun ppf -> Format.fprintf ppf " for script %s"))
        script_name
      >>= fun () -> return_unit
  | Error errs -> on_errors errs

let list_contract_unreachables_and_entrypoints cctxt ~chain ~block ~contract =
  Alpha_services.Contract.list_entrypoints cctxt (chain, block) contract

let list_contract_unreachables cctxt ~chain ~block ~contract =
  list_contract_unreachables_and_entrypoints cctxt ~chain ~block ~contract
  >>=? fun (unreachables, _) -> return unreachables

let list_contract_entrypoints cctxt ~chain ~block ~contract =
  list_contract_unreachables_and_entrypoints cctxt ~chain ~block ~contract
  >>=? fun (_, entrypoints) ->
  if not @@ List.mem_assoc ~equal:String.equal "default" entrypoints then
    contract_entrypoint_type cctxt ~chain ~block ~contract ~entrypoint:"default"
    >>= function
    | Ok (Some ty) -> return (("default", ty) :: entrypoints)
    | Ok None -> return entrypoints
    | Error _ as err -> Lwt.return err
  else return entrypoints

let list_unreachables cctxt ~chain ~block (program : Script.expr) =
  Plugin.RPC.Scripts.list_entrypoints cctxt (chain, block) ~script:program
  >>=? fun (unreachables, _) -> return unreachables

let list_entrypoints cctxt ~chain ~block (program : Script.expr) =
  Plugin.RPC.Scripts.list_entrypoints cctxt (chain, block) ~script:program
  >>=? fun (_, entrypoints) ->
  if not @@ List.mem_assoc ~equal:String.equal "default" entrypoints then
    script_entrypoint_type cctxt ~chain ~block program ~entrypoint:"default"
    >>= function
    | Ok (Some ty) -> return (("default", ty) :: entrypoints)
    | Ok None -> return entrypoints
    | Error _ as err -> Lwt.return err
  else return entrypoints

let print_entrypoints_list (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name = function
  | Ok entrypoint_list ->
      (if emacs then
         cctxt#message
           "@[<v 2>(@[%a@])@."
           (Format.pp_print_list
              ~pp_sep:Format.pp_print_cut
              (fun ppf (entrypoint, ty) ->
                Format.fprintf
                  ppf
                  "@[<v 2>( ( entrypoint . %s ) ( type . @[%a@]))@]"
                  entrypoint
                  Michelson_v1_emacs.print_expr
                  ty))
           entrypoint_list
       else
         cctxt#message
           "@[<v 2>Entrypoints%a%a: @,%a@]@."
           (Format.pp_print_option (fun ppf ->
                Format.fprintf ppf " for contract %a" Contract.pp))
           contract
           (Format.pp_print_option (fun ppf ->
                Format.fprintf ppf " for script %s"))
           script_name
           (Format.pp_print_list
              ~pp_sep:Format.pp_print_cut
              (fun ppf (entrypoint, ty) ->
                Format.fprintf
                  ppf
                  "@[<v 2>%s: @[%a@]@]"
                  entrypoint
                  Michelson_v1_printer.print_expr
                  ty))
           entrypoint_list)
      >>= fun () -> return_unit
  | Error errs -> on_errors errs

let print_unreachables (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name = function
  | Ok unreachable ->
      (if emacs then
         cctxt#message
           "@[<v 2>(@[%a@])@."
           (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf path ->
                Format.fprintf
                  ppf
                  "@[<h>( unreachable-path . %a )@]"
                  (Format.pp_print_list
                     ~pp_sep:Format.pp_print_space
                     (fun ppf prim ->
                       Format.pp_print_string ppf
                       @@ Michelson_v1_primitives.string_of_prim prim))
                  path))
           unreachable
       else
         match unreachable with
         | [] -> cctxt#message "@[<v 2>None.@]@."
         | _ ->
             cctxt#message
               "@[<v 2>Unreachable paths in the argument%a%a: @[%a@]@."
               (Format.pp_print_option (fun ppf ->
                    Format.fprintf ppf " of contract %a" Contract.pp))
               contract
               (Format.pp_print_option (fun ppf ->
                    Format.fprintf ppf " of script %s"))
               script_name
               (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf ->
                    Format.fprintf
                      ppf
                      "@[<h> %a @]"
                      (Format.pp_print_list
                         ~pp_sep:(fun ppf _ -> Format.pp_print_string ppf "/")
                         (fun ppf prim ->
                           Format.pp_print_string ppf
                           @@ Michelson_v1_primitives.string_of_prim prim))))
               unreachable)
      >>= fun () -> return_unit
  | Error errs -> on_errors errs
