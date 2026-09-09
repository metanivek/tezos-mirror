(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Reveal_hash = Tezos_raw_protocol_alpha.Sc_rollup_reveal_hash

type error += Invalid_preimage_for_hash of Hex.t * string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_invalid_preimage"
    ~title:"Preimage has not the expected hash"
    ~description:
      "The EVM node could not apply a blueprint on top of its local EVM state."
    ~pp:(fun ppf (hash, _preimage) ->
      Format.fprintf
        ppf
        "The preimage received for %s doesn't return the same hash"
        hash)
    Data_encoding.(obj2 (req "expected_hash" string) (req "preimage" string))
    (function
      | Invalid_preimage_for_hash (`Hex hash, preimage) -> Some (hash, preimage)
      | _ -> None)
    (fun (hash, preimage) -> Invalid_preimage_for_hash (`Hex hash, preimage))

type preimages = Contents of bytes | Hashes of string list

let preimages_encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"content"
          (Tag 0)
          bytes
          (function Contents _payload -> assert false | _ -> None)
          (fun payload -> Contents payload);
        case
          ~title:"hashes"
          (Tag 1)
          (list Reveal_hash.encoding)
          (function Hashes _hashes -> assert false | _ -> None)
          (fun hashes -> Hashes (List.map Reveal_hash.to_hex hashes));
      ])

let check_preimage (`Hex hash) preimage =
  let computed_hash =
    Reveal_hash.hash_string ~scheme:Reveal_hash.Blake2B [preimage]
    |> Reveal_hash.to_hex
  in
  hash = computed_hash

let delete_preimage preimages (`Hex hash) =
  Lwt_unix.unlink (Filename.concat preimages hash)

let rec reveal_and_check ~preimages_endpoint ~preimages ~num_download_retries
    hash =
  let open Lwt_result_syntax in
  let*! preimage =
    Octez_smart_rollup_wasm_debugger_lib.Commands.reveal_preimage
      ~preimages_endpoint
      ~preimages
      0
      (* Retrying makes sense only when the preimage is fed through stdin, this
         wouldn't make sense in our case. *)
      hash
  in
  if not (check_preimage hash preimage) then
    if num_download_retries <= 0 then
      tzfail (Invalid_preimage_for_hash (hash, preimage))
    else
      let*! () = delete_preimage preimages hash in
      reveal_and_check
        ~preimages_endpoint
        ~preimages
        ~num_download_retries:(pred num_download_retries)
        hash
  else return preimage

(* The preimages of a kernel form a balanced tree: the content pages holding
   the kernel itself are the leaves, and each level above them lists the hashes
   of the level below (see [prepare_preimages] in the kernel SDK). Walking that
   tree breadth-first therefore reaches every hash page before the first
   content page, so the number of preimages the kernel is made of is known as
   soon as a content page is reached: the hash pages downloaded so far, plus
   the content pages of the last level. *)
let download ~preimages_endpoint ~preimages ~(root_hash : Hex.t)
    ?(num_download_retries = 1) ?(progress = false) () =
  let open Lwt_result_syntax in
  let fetch hash =
    let* preimage =
      reveal_and_check ~preimages_endpoint ~preimages ~num_download_retries hash
    in
    return (Data_encoding.Binary.of_string_exn preimages_encoding preimage)
  in
  let hashes_of_page hashes = List.map (fun hash -> `Hex hash) hashes in
  (* Downloads the levels of hash pages, from the root down. Returns how many
     pages were downloaded, together with the hashes of the last level, which
     are the content pages. *)
  let rec download_hash_pages ~downloaded ~level ~next_level =
    match (level, next_level) with
    | [], [] -> return (downloaded, [])
    | [], _ :: _ ->
        (download_hash_pages [@tailcall])
          ~downloaded
          ~level:(List.rev next_level)
          ~next_level:[]
    | hash :: level, _ -> (
        let* page = fetch hash in
        let downloaded = downloaded + 1 in
        match page with
        | Hashes page_hashes ->
            (download_hash_pages [@tailcall])
              ~downloaded
              ~level
              ~next_level:
                (List.rev_append (hashes_of_page page_hashes) next_level)
        | Contents _content -> return (downloaded, level))
  in
  (* Downloads the content pages of the last level, reporting each one. A page
     of hashes is not expected here, but is walked rather than dropped so that
     an unbalanced tree yields a complete kernel instead of a silently missing
     one -- only the total the progress bar counts towards is then off. *)
  let rec download_content_pages report = function
    | [] -> return_unit
    | hash :: hashes -> (
        let* page = fetch hash in
        let*! () = report 1 in
        match page with
        | Contents _content ->
            (download_content_pages [@tailcall]) report hashes
        | Hashes page_hashes ->
            (download_content_pages [@tailcall])
              report
              (List.rev_append (hashes_of_page page_hashes) hashes))
  in
  let* downloaded, content_pages =
    let listing =
      download_hash_pages ~downloaded:0 ~level:[root_hash] ~next_level:[]
    in
    if progress then
      Progress_bar.Lwt.with_background_spinner
        ~no_tty_quiet:true
        ~message:"Listing the preimages of the kernel"
        listing
    else listing
  in
  let* () =
    if progress then
      let total = downloaded + List.length content_pages in
      Progress_bar.Lwt.with_reporter
        (Progress_bar.progress_bar
           ~update_interval:0.5
           ~message:"Downloading kernel"
           ~counter:`Int
           total)
        (fun report ->
          let*! () = report downloaded in
          download_content_pages report content_pages)
    else download_content_pages (fun _ -> Lwt.return_unit) content_pages
  in
  let*! () = Events.predownload_kernel root_hash in
  return_unit
