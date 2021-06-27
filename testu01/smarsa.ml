external birthday_spacings : Unif01.gen -> Sres.poisson option -> int -> int ->
  int -> int -> int -> int -> unit =
  "camlbytecode_smarsa_BirthdaySpacings" "caml_smarsa_BirthdaySpacings"

(** [birthday_spacings gen res N n r d t p] implements the birthday spacings
   test proposed in Marsaglia 1985 ({i A current view of random number
   generators}) and studied further by Knuth (1998, {i The Art of Computer
   Programming, Volume 2: Seminumerical Algorithms}) and L’Ecuyer and Simard
   (2001, {i On the performance of birthday spacings tests for certain families
   of random number generators}). This is a variation of the collision test, in
   which [n] points are thrown into [k = d^t] cells in [t] dimensions as in
   {!Smultin.multinomial}. The cells are numbered from 0 to [k−1]. To generate a
   point, [t] integers [y0],…,[y_{t−1}] in [{0, …, d−1}] are generated from [t]
   successive uniforms. The parameter [p] decides in which order these [t]
   integers are used to determine the cell number: The cell number is [c = y0
   d^{t−1} + ··· + y_{t−2} d + y_{t−1}] if [p = 1] and [c = y_{t−1} d^{t−1} +
   ··· + y1 d + y0] if [p = 2]. This corresponds to using
   {!Smultin.gener_cell_serial} for [p = 1] and {!Smultin.gener_cell_serial_2}
   for [p = 2].

    The points obtained can be viewed as [n] birth dates in a year of [k] days
   (Altman 1988, {i Bit-wise behavior of random number generators}, Marsaglia
   1985). Let [I1 ≤ I2 ≤ ··· ≤ In] be the [n] cell numbers obtained, sorted in
   increasing order. The test computes the differences [I_{j+1} − Ij], for [1 ≤
   j < n], and count the number [Y] of collisions between these differences.
   Under H0, Y is approximately Poisson with mean [λ = n³/(4k)], and the sum of
   all values of [Y] for the [N] replications (the total number of collisions)
   is approximately Poisson with mean [Nλ]. The test computes this total number
   of collisions and computes the corresponding p-value using the Poisson
   distribution. Recommendation: [k] should be very large and [λ] relatively
   small. Restrictions: k ≤ {!Smarsa.max_k}, [8Nλ ≤ k^{1/4}] or [4n ≤
   k^{5/12}/N^{1/3}], and [p ∈ {1,2}]. *)
