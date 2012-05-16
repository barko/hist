open Histogram
open Printf

let _ = 
  let min_value = ref None in
  let max_value = ref None in
  let norm_spec = ref Count in
  let bin_spec = ref (NumBins 10) in
  let in_file_name = ref None in
  let out_file_name = ref None in

  let usage = sprintf "usage: %s [OPTIONS] [in-file-name] [out-file-name]" Sys.argv.(0) in


  Arg.parse [
    "-min-value", Arg.Float (fun f -> min_value := Some f),
    "  minimum value of the histogram";

    "-max-value", Arg.Float (fun f -> max_value := Some f),
    "  maximum value of the histogram";

    "-area", Arg.Unit (fun () -> norm_spec := Area),
    "  normalize the histogram so the sum of the bins equals one";

    "-max", Arg.Unit (fun () -> norm_spec := Max),
    "  normalize the histogram so the maximum bin equals one";

    "-count", Arg.Unit (fun () -> norm_spec := Count),
    "  no bin normalization";

    "-num-bins", Arg.Int (fun i -> bin_spec := NumBins i),
    "  the number of bins in the histogram";

    "-bin-width", Arg.Float (fun bw -> bin_spec := BinWidth bw),
    "  the width of each bin";

    "-i", Arg.String (fun s -> in_file_name := Some s),
    "  the input file path; stdin is used if not specified";

    "-o", Arg.String (fun s -> in_file_name := Some s),
    "  the output file path; stdout is used if not specified"

  ] (fun _ -> ()) usage;

  let in_channel = 
    match !in_file_name with
      | None -> stdin
      | Some path -> open_in path
  in

  let out_channel = 
    match !out_file_name with
      | None -> stdout
      | Some path -> open_out path
  in

  (* parse the values from the file *)
  let values = Mikmatch.Text.map_lines_of_channel float_of_string in_channel in

  let h = create 
    ?min_value:!min_value
    ?max_value:!max_value 
    ~bin_spec:!bin_spec 
    ~norm_spec:!norm_spec 
    values 
  in

  for b = 0 to h.num_bins-1 do
    let bin_center = ((float b) +. 0.5) *. h.bin_width in
    fprintf out_channel "%e\t%e\n" bin_center h.bins.(b)
  done;

  close_in in_channel;
  close_out out_channel;
  ()

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
