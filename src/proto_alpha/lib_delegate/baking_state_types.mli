(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Key type and functions}

    Used for both consensus keys and companion keys. *)
module Key_id : sig
  type t

  (** Only use at library frontiers *)
  val to_pkh : t -> Signature.public_key_hash

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Table : sig
    include Hashtbl.SeededS with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end
end

module Key : sig
  type t = private {
    alias : string;
    id : Key_id.t;
    public_key : Signature.public_key;
    secret_key_uri : Client_keys.sk_uri;
  }

  val make :
    alias:string ->
    public_key:Signature.public_key ->
    public_key_hash:Signature.public_key_hash ->
    secret_key_uri:Client_keys.sk_uri ->
    t

  (** Partial encoding for {!t} that omits the secret key to avoid
      leaking it in event logs (because {!Client_keys.sk_uri} contains
      the plain secret key when the key is unencrypted).

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_bls : t -> bool

  module Set : Set.S with type elt = t
end

(** {2 Delegates slots type and functions} *)
module Delegate_id : module type of Key_id

module Delegate : sig
  type manager_key

  type t = private {
    manager_key : manager_key;
    consensus_key : Key.t;
    companion_key : Key.t option;
  }

  val delegate_id : t -> Delegate_id.t

  (** Partial encoding for {!t} that omits secret keys to avoid
      leaking them in event logs; see
      {!Key.encoding_for_logging__cannot_decode}.

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  (** Prints the manager key and consensus key but not the companion
      key. *)
  val pp_without_companion_key : Format.formatter -> t -> unit

  (** Builds a {!t} from an element of the output of
      {!Plugin.RPC.Validators.get}, if the consensus key is present in
      [known_keys]; otherwise, returns [None].

      If the consensus key is a known BLS key and the validator
      argument contains a companion key but that companion key is not
      in [known_keys], emits an error event but nevertheless returns a
      {!t} where [companion_key = None]. (This function is in Lwt to
      be able to emit this event.) *)
  val of_validator : known_keys:Key.Set.t -> RPC.Validators.t -> t option Lwt.t
end
