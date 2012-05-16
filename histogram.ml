type bin_spec = 
  | NumBins of int (* specify number of bins *)
  | BinWidth of float (* or, specify the width of each bin *)

type norm_spec = 
  | Count (* no normalization *)
  | Area (* sum of bin counts equals 1 *)
  | Max (* maximum bin count equals 1 *)

let rec extremes_and_count mn mx c = function
  | [] ->
      mn, mx, c

  | h :: t ->
      let mn = min mn h in
      let mx = max mx h in
      extremes_and_count mn mx (c+1) t


type t = {
  bins : float array;
  minimum : float;
  bin_width : float;
  num_bins : int (* equals to [Array.length bins] *)
}

exception Error of string

let create ?min_value ?max_value ?(bin_spec=(NumBins 10)) ?(norm_spec=Count) list =
  (* identify the extremes of the values in [list] *)
  let mn, mx, length = extremes_and_count infinity neg_infinity 0 list in

  if length = 0 then
    raise (Error "input is empty");

  (* lower the minimum if indicated by [min_value] *)
  let mn = 
    match min_value with 
      | Some mv -> min mv mn
      | None -> mn
  in

  (* raise the maximum if indicated by [min_value] *)
  let mx = 
    match max_value with 
      | Some mv -> max mv mx
      | None -> mx
  in

  let range = mx -. mn in

  let num_bins, bin_width =
    match bin_spec with
      | NumBins nb when nb < 1 ->
          raise (Error "number of bins is not positive")

      | NumBins nb -> 
          nb, range /. float nb

      | BinWidth bw when bw <= 0.0 ->
          raise (Error "bin width is not positive")

      | BinWidth bw ->
          let nb = truncate (ceil (range /. bw)) in
          let bw = range /. (float nb) in
          nb, bw
  in

  let num_bins_1 = num_bins - 1 in

  let bin_of_value v =
    let bin = truncate ((v -. mn) /. bin_width) in
    min bin num_bins_1
  in

  let bins = Array.create num_bins 0.0 in

  List.iter (
    fun value ->
      let bin = bin_of_value value in
      bins.(bin) <- bins.(bin) +. 1.
  ) list;

  (match norm_spec with
     | Count -> 
         (* nothing to do *)
         () 

     | Area ->
         (* make the sum of the values in each bin equal to one *)
         let length = float length in
         for i = 0 to num_bins-1 do
           bins.(i) <- bins.(i) /. length;
         done

     | Max ->
         (* make the value bin with maximal count equal to one; first
            find that bin *)
         let max_bin, max_bin_value, _ = Array.fold_left (
           fun (max_bin, max_bin_value, bin) bin_value ->
             if bin_value > max_bin_value then
               bin, bin_value, bin+1
             else
               (* unchanged *)
               max_bin, max_bin_value, bin+1
         ) (-1, neg_infinity, 0) bins in

         for i = 0 to num_bins-1 do
           bins.(i) <- bins.(i) /. max_bin_value
         done;

  );
  {
    bins;
    minimum = mn;
    bin_width;
    num_bins
  }



(*  
Copyright (c) 2012, barko 00336ea19fcb53de187740c490f764f4
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
      
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of barko nor the names of contributors may be used
   to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)
