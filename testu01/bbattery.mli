(** {1 Bbattery}

   This module contains predefined batteries of statistical tests for sources of
   random bits or sequences of uniform random numbers in the interval \[0,1). To
   test a RNG for general use, one could first apply the small and fast battery
   {!SmallCrush}. If it passes, one could then apply the more stringent battery
   {!Crush}, and finally the yet more time-consuming battery {!BigCrush}.The
   batteries {!Alphabit} and {!Rabbit} can be applied on a binary file
   considered as a source of random bits. They can also be applied on a
   programmed generator. {!Alphabit} has been defined primarily to test hardware
   random bits generators. The battery {!PseudoDIEHARD} applies most of the
   tests in the well-known DIEHARD suite of Marsaglia. The battery {!FIPS-140-2}
   implements the small suite of tests of the FIPS-140-2 standard from NIST.

   The batteries described in this module will write the results of each test
   (on standard output) with a standard level of details (assuming that the
   boolean switches of module {!Swrite} have their default values), followed by
   a summary report of the suspect p-values obtained from the specific tests
   included in the batteries. It is also possible to get only the summary report
   in the output, with no detailed output from the tests, by setting the boolean
   switch {!Swrite.set_basic} to [false].

   Some of the tests compute more than one statistic using the same stream of
   random numbers and these statistics are thus not independent. That is why the
   number of statistics in the summary reports is larger than the number of
   tests in the description of the batteries. *)

val get_n_tests : unit -> int
(** The number of p-values in the array {!get_p_val}. For small sample size,
   some of the tests in the battery may not be done. Furthermore, some of the
   tests computes more than one statistic and its p-value, so {!get_n_tests}
   will usually be larger than the number of tests in the battery. *)

val get_p_val : unit -> float array
(** This array keeps the p-values resulting from the battery of tests that is
   currently applied (or the last one that has been called). It is used by any
   battery in this module. The p-value of the [j]-th test in the battery is kept
   in [(get_p_val()).(j-1])], for [1 ≤ j ≤ get_n_tests()].

   Note for the bindings: calling {!get_p_val} copies the underlying C array up
   to [get_n_tests()]. This means that one can modify the result of
   [get_p_val()] safely, and that an other call of [get_p_val()] will yield the
   unmodified array again. This also means that, contrary to C, the array
   returned by [get_p_val()] contains exactly [get_n_tests()] elements. It is
   then safe to iterate through it with the usual functions. *)

val get_test_names : unit -> string array
(** This array keeps the names of each test from the battery that is currently
   applied (or the last one that has been called). It is used by any battery in
   this module. The name of the [j]-th test in the battery is kept in
   [(get_test_names()).(j-1)], for [1 ≤ j ≤ get_n_tests()].

   Note for the bindings: see {!get_p_val}. *)

(** {2 SmallCrush} *)

val small_crush : Unif01.gen -> unit
val small_crush_file : string -> unit

(** Both functions applies [SmallCrush], a small and fast battery of tests, to a
   RNG. The function {!small_crush_file} applies [SmallCrush] to a RNG given as
   a text file of floating-point numbers in \[0,1); the file requires slightly
   less than 51320000 random numbers. The file will be rewound to the beginning
   before each test. Thus {!small_crush} applies the tests on one unbroken
   stream of successive numbers, while {!small_crush_file} applies each test on
   the same sequence of numbers. Some of these tests assume that the generator
   returns at least 30bits of resolution; if this is not the case, then the
   generator is most likely to fail these particular tests.

    The following tests are applied:
    + {!Smarsa.birthday_spacings} with N = 1, n = 5×10⁶, r = 0, d = 2³⁰, t = 2, p = 1.
    + {!Sknuth.collision}         with N = 1, n = 5×10⁶, r = 0, d = 2¹⁶, t = 2.
    + {!Sknuth.gap}               with N = 1, n = 2×10⁵, r = 22, Alpha = 0, Beta = 1/256.
    + {!Sknuth.simp_poker}        with N = 1, n = 4×10⁵, r = 24, d = 64, k = 64.
    + {!Sknuth.coupon_collector}  with N = 1, n = 5×10⁵, r = 26, d = 16.
    + {!Sknuth.max_oft}           with N = 1, n = 2×10⁶, r = 0, d = 10⁵, t = 6.
    + {!Svaria.weight_distrib}    with N = 1, n = 2×10⁵, r = 27, k = 256, Alpha = 0, Beta = 1/8.
    + {!Smarsa.matrix_rank}       with N = 1, n = 20000, r = 20, s = 10, L = k = 60.
    + {!Sstring.hamming_indep}    with N = 1, n= 5×10⁵, r = 20, s = 10, L = 300, d = 0.
    + {!Swalk.random_walk_1}      with N = 1, n = 10⁶, r = 0, s = 30, L₀ = 150, L₁ = 150. *)

val repeat_small_crush : Unif01.gen -> int array -> unit

(** [repeat_small_crush gen rep] applies specific tests of [SmallCrush] on
    generator [gen]. Test numbered {i i} in the enumeration above will be applied
    [rep[i]] times successively on [gen]. Those tests with [rep[i]] = 0 will not
    be applied. This is useful when a test in [SmallCrush] had a suspect p-value,
    and one wants to reapply the test a few more times to find out whether the
    generator failed the testor whether the suspect p-value was a statistical
    fluke. Restriction: Array [rep] must have one more element than the number of
    tests in [SmallCrush]. *)

val ntests_small_crush : int
(** Number of tests in [SmallCrush]: 10. *)

(** {2 Crush} *)

val crush : Unif01.gen -> unit

(** Applies the battery [Crush], a suite of stringent statistical tests, to the
   generator [gen]. The battery includes the classical tests described in Knuth
   1998 ({i The Art of Computer Programming, Volume 2: Seminumerical
   Algorithms}) as well as many other tests. Some of these tests assume that
   [gen] returns at least 30 bits of resolution; if that is not the case, then
   the generator will certainly fail these particular tests. One test requires
   31 bits of resolution: the [BirthdaySpacings] test with t = 2. On a PC with
   an AMD Athlon 64 Processor 4000+ of clock speed 2400 MHz running with Red Hat
   Linux, [Crush] will require around 1 hour of CPU time. [Crush] uses
   approximately 2^35 random numbers. The following tests are applied: FIXME *)

val repeat_crush : Unif01.gen -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Crush]. *)

val ntests_crush : int
(** Number of tests in [Crush]: 96. *)

(** {2 BigCrush} *)

val big_crush : Unif01.gen -> unit

(** Applies the battery [BigCrush], a suite of very stringent statistical tests,
   to the generator [gen]. Some of these tests assume that [gen] returns at
   least 30 bits of resolution; if that is not the case, then the generator will
   certainly fail these particular tests. One test requires 31 bits of
   resolution: the [BirthdaySpacings] test with t = 2. On a PC with an AMD
   Athlon 64 Processor 4000+ of clock speed 2400 MHz running with Linux,
   [BigCrush] will take around 8 hours of CPUtime. [BigCrush] uses close to 2^38
   random numbers. The following tests are applied:

   + {!Smarsa.serial_over} with N = 1, n = 10^9, r = 0, d = 2^8, t = 3.
   + {!Smarsa.serial_over} with N = 1, n = 10^9, r = 22, d = 2^8, t = 3.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 0, d = 2^21, t = 2.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 9, d = 2^21, t = 2.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 0, d = 2^14, t = 3.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 16, d = 2^14, t = 3.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 0, d = 64, t = 7.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 24, d = 64, t = 7.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 0, d = 8, t = 14.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 27, d = 8, t = 14.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 0, d = 4, t = 21.
   + {!Smarsa.collision_over} with N = 30, n= 2∗10^7, r = 28, d = 4, t = 21.
   + {!Smarsa.birthday_spacings} with N = 100, n = 10^7, r = 0, d = 2^31, t = 2, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 2∗10^7, r = 0, d = 2^21, t = 3, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 3∗10^7, r = 14, d = 2^16, t = 4, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 2∗10^7, r = 0, d = 2^9, t = 7, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 2∗10^7, r = 7, d = 2^9, t = 7, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 3∗10^7, r = 14, d = 2^8, t = 8, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 3∗10^7, r = 22, d = 2^8, t = 8, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 3∗10^7, r = 0, d = 2^4, t = 16, p = 1.
   + {!Smarsa.birthday_spacings} with N = 20, n = 3∗10^7, r = 26, d = 2^4, t = 16, p = 1.
   + {!Snpair.close_pairs} with N = 30, n = 6∗10^6, r = 0, t = 3, p = 0, m = 30.
   + {!Snpair.close_pairs} with N = 20, n = 4∗10^6, r = 0, t = 5, p = 0, m = 30.
   + {!Snpair.close_pairs} with N = 10, n = 3∗10^6, r = 0, t = 9, p = 0, m = 30.
   + {!Snpair.close_pairs} with N = 5, n = 2∗10^6, r = 0, t = 16, p = 0, m = 30.
   + {!Sknuth.simp_poker} with N = 1, n = 4∗10^8, r = 0, d = 8, k = 8.
   + {!Sknuth.simp_poker} with N = 1, n = 4∗10^8, r = 27, d = 8, k = 8.
   + {!Sknuth.simp_poker} with N = 1, n = 10^8, r = 0, d = 32, k = 32.
   + {!Sknuth.simp_poker} with N = 1, n = 10^8, r = 25, d = 32, k = 32.
   + {!Sknuth.coupon_collector} with N = 1, n = 2∗10^8, r = 0, d = 8.
   + {!Sknuth.coupon_collector} with N = 1, n = 2∗10^8, r = 10, d = 8.
   + {!Sknuth.coupon_collector} with N = 1, n = 2∗10^8, r = 20, d = 8.
   + {!Sknuth.coupon_collector} with N = 1, n = 2∗10^8, r = 27, d = 8.
   + {!Sknuth.gap} with N = 1, n = 5∗10^8, r = 0, Alpha = 0, Beta = 1/16.
   + {!Sknuth.gap} with N = 1, n = 3∗10^8, r = 25, Alpha = 0, Beta = 1/32.
   + {!Sknuth.gap} with N = 1, n = 10^8, r = 0, Alpha = 0, Beta = 1/128.
   + {!Sknuth.gap} with N = 1, n = 10^7, r = 20, Alpha = 0, Beta = 1/1024.
   + {!Sknuth.run} with N = 5, n = 10^9, r = 0, Up = FALSE.
   + {!Sknuth.run} with N = 5, n = 10^9, r = 15, Up = TRUE.
   + {!Sknuth.permutation} with N = 1, n = 10^9, r = 0, t = 3.
   + {!Sknuth.permutation} with N = 1, n = 10^9, r = 0, t = 5.
   + {!Sknuth.permutation} with N = 1, n = 5∗10^8, r = 0, t = 7.
   + {!Sknuth.permutation} with N = 1, n = 5∗10^8, r = 10, t = 10.
   + {!Sknuth.collision_permut} with N = 20, n = 2∗10^7, r = 0, t = 14.
   + {!Sknuth.collision_permut} with N = 20, n = 2∗10^7, r = 10, t = 14.
   + {!Sknuth.max_oft} with N = 40, n = 10^7, r = 0, d = 10^5, t = 8.
   + {!Sknuth.max_oft} with N = 30, n = 10^7, r = 0, d = 10^5, t = 16.
   + {!Sknuth.max_oft} with N = 20, n = 10^7, r = 0, d = 10^5, t = 24.
   + {!Sknuth.max_oft} with N = 20, n = 10^7, r = 0, d = 10^5, t = 32.
   + {!Svaria.sample_prod} with N = 40, n = 10^7, r = 0, t = 8.
   + {!Svaria.sample_prod} with N = 20, n = 10^7, r = 0, t = 16.
   + {!Svaria.sample_prod} with N = 20, n = 10^7, r = 0, t = 24.
   + {!Svaria.sample_mean} with N = 2∗10^7, n = 30, r = 0.
   + {!Svaria.sample_mean} with N = 2∗10^7, n = 30, r = 10.
   + {!Svaria.sample_corr} with N = 1, n = 2∗10^9, r = 0, k = 1.
   + {!Svaria.sample_corr} with N = 1, n = 2∗10^9, r = 0, k = 2.
   + {!Svaria.appearance_spacings} with N = 1, Q = 10^7, K = 10^9, r = 0, s = 3, L = 15.
   + {!Svaria.appearance_spacings} with N = 1, Q = 10^7, K = 10^9, r = 27, s = 3, L = 15.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 0, k = 256, Alpha = 0, Beta = 1/4.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 20, k = 256, Alpha = 0, Beta = 1/4.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 28, k = 256, Alpha = 0, Beta = 1/4.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 0, k = 256, Alpha = 0, Beta = 1/16.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 10, k = 256, Alpha = 0, Beta = 1/16.
   + {!Svaria.weight_distrib} with N = 1, n = 2∗10^7, r = 26, k = 256, Alpha = 0, Beta = 1/16.
   + {!Svaria.sum_collector} with N = 1, n = 5∗10^8, r = 0, g = 10.
   + {!Smarsa.matrix_rank} with N = 10, n = 10^6, r = 0, s = 5, L = k = 30.
   + {!Smarsa.matrix_rank} with N = 10, n = 10^6, r = 25, s = 5, L = k = 30.
   + {!Smarsa.matrix_rank} with N = 1, n = 5000, r = 0, s = 4, L = k = 1000.
   + {!Smarsa.matrix_rank} with N = 1, n = 5000, r = 26, s = 4, L = k = 1000.
   + {!Smarsa.matrix_rank} with N = 1, n = 80, r = 15, s = 15, L = k = 5000.
   + {!Smarsa.matrix_rank} with N = 1, n = 80, r = 0, s = 30, L = k = 5000.
   + {!Smarsa.savir2} with N = 10, n = 10^7, r = 10, m = 220, t = 30.
   + {!Smarsa.gcd} with N = 10, n = 5∗10^7, r = 0, s = 30.
   + {!Swalk.random_walk_1} with N = 1, n = 10^8, r = 0, s = 5, L0 = L1 = 50.
   + {!Swalk.random_walk_1} with N = 1, n = 10^8, r = 25, s = 5, L0 = L1 = 50.
   + {!Swalk.random_walk_1} with N = 1, n = 10^7, r = 0, s = 10, L0 = L1 = 1000.
   + {!Swalk.random_walk_1} with N = 1, n = 10^7, r = 20, s = 10, L0 = L1 = 1000.
   + {!Swalk.random_walk_1} with N = 1, n = 10^6, r = 0, s = 15, L0 = L1 = 10000.
   + {!Swalk.random_walk_1} with N = 1, n = 10^6, r = 15, s = 15, L0 = L1 = 10000.
   + {!Scomp.linear_comp} with N = 1, n = 400000, r = 0, s = 1.
   + {!Scomp.linear_comp} with N = 1, n = 400000, r = 29, s = 1.
   + {!Scomp.lempel_ziv} with N = 10, k = 27, r = 0, s = 30.
   + {!Scomp.lempel_ziv} with N = 10, k = 27, r = 15, s = 15.
   + {!Sspectral.fourier_3} with N = 100000, r = 0, s = 3, k = 14.
   + {!Sspectral.fourier_3} with N = 100000, r = 27, s = 3, k = 14.
   + {!Sstring.longest_head_run} with N = 1, n = 1000, r = 0, s = 3, L = 10^7.
   + {!Sstring.longest_head_run} with N = 1, n = 1000, r = 27, s = 3, L = 10^7.
   + {!Sstring.periods_in_strings} with N = 10, n = 5∗10^8, r = 0, s = 10.
   + {!Sstring.periods_in_strings} with N = 10, n = 5∗10^8, r = 20, s = 10.
   + {!Sstring.hamming_weight_2} with N = 10, n = 10^9, r = 0, s = 3, L = 10^6.
   + {!Sstring.hamming_weight_2} with N = 10, n = 10^9, r = 27, s = 3, L = 10^6.
   + {!Sstring.hamming_corr} with N = 1, n = 10^9, r = 10, s = 10, L = 30.
   + {!Sstring.hamming_corr} with N = 1, n = 10^8, r = 10, s = 10, L = 300.
   + {!Sstring.hamming_corr} with N = 1, n = 10^8, r = 10, s = 10, L = 1200.
   + {!Sstring.hamming_indep} with N = 10, n = 3∗10^7, r = 0, s = 3, L = 30, d = 0.
   + {!Sstring.hamming_indep} with N = 10, n = 3∗10^7, r = 27, s = 3, L = 30, d = 0.
   + {!Sstring.hamming_indep} with N = 1, n = 3∗10^7, r = 0, s = 4, L = 300, d = 0.
   + {!Sstring.hamming_indep} with N = 1, n = 3∗10^7, r = 26, s = 4, L = 300, d = 0.
   + {!Sstring.hamming_indep} with N = 1, n = 10^7, r = 0, s = 5, L = 1200, d = 0.
   + {!Sstring.hamming_indep} with N = 1, n = 10^7, r = 25, s = 5, L = 1200, d = 0.
   + {!Sstring.run} with N = 1, n = 2∗10^9, r = 0, s = 3.
   + {!Sstring.run} with N = 1, n = 2∗10^9, r = 27, s = 3.
   + {!Sstring.auto_cor} with N = 10, n = 10^9, r = 0, s = 3, d = 1.
   + {!Sstring.auto_cor} with N = 10, n = 10^9, r = 0, s = 3, d = 3.
   + {!Sstring.auto_cor} with N = 10, n = 10^9, r = 27, s = 3, d = 1.
   + {!Sstring.auto_cor} with N = 10, n = 10^9, r = 27, s = 3, d = 3. *)

val repeat_big_crush : Unif01.gen -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [BigCrush]. *)

val ntests_big_crush : int
(** Number of tests in [BigCrush]: 106. *)

(** {2 Rabbit} *)

val rabbit : Unif01.gen -> float -> unit
(** Applies the [Rabbit] battery of tests to the generator [gen] using at most
   [nb] bits for each test. See the description of the tests in {!rabbit_file}. *)

val rabbit_file : string -> float -> unit

(** Applies the [Rabbit] battery of tests to the first [nb] bits (or less, if
   [nb] is too large) of the binary file [filename]. For each test, the file is
   reset and the test is applied to the bit stream starting at the beginning of
   the file. The bits themselves are processed in nearly all the tests as blocks
   of 32 bits (unsigned integers); the two exceptions are
   {!Svaria.appearance_spacings}, which uses blocks of 30 bits (and discards the
   last 2 bits out of each block of 32), and {!Sstring.periods_in_strings} which
   uses blocks of 31 bits (and discards 1 bit out of 32). The parameters of each
   test are chosen automatically as a function of [nb], in order to make the
   test reasonably sensitive. On a PC with an Athlon processor of clock speed
   1733 MHz running under Linux, [Rabbit] will takeabout 5 seconds to test a
   stream of 220bits, 90 seconds to test a stream of 225bits, and 28 minutes to
   test a stream of 230bits. Restriction: [nb] ≥ 500.

   + {!Smultin.multinomial_bits_over}
   + {!Snpair.close_pairs_bit_match} in t = 2 dimensions
   + {!Snpair.close_pairs_bit_match} in t = 4 dimensions.
   + {!Svaria.appearance_spacings}
   + {!Scomp.linear_comp}
   + {!Scomp.lempel_ziv}
   + {!Sspectral.fourier_1}
   + {!Sspectral.fourier_3}
   + {!Sstring.longest_head_run}
   + {!Sstring.periods_in_strings}
   + {!Sstring.hamming_weight} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 64 bits.
   + {!Sstring.hamming_corr} with blocks of L = 128 bits.
   + {!Sstring.hamming_indep} with blocks of L = 16 bits.
   + {!Sstring.hamming_indep} with blocks of L = 32 bits.
   + {!Sstring.hamming_indep} with blocks of L = 64 bits.
   + {!Sstring.auto_cor} with a lag d = 1.
   + {!Sstring.auto_cor} with a lag d = 2.
   + {!Sstring.run}
   + {!Smarsa.matrix_rank} with 32×32 matrices.
   + {!Smarsa.matrix_rank} with 320×320 matrices.
   + {!Smarsa.matrix_rank} with 1024×1024 matrices.
   + {!Swalk.random_walk_1} with walks of length L= 128.
   + {!Swalk.random_walk_1} with walks of length L= 1024.
   + {!Swalk.random_walk_1} with walks of length L= 10016. *)

val repeat_rabbit : Unif01.gen -> float -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Rabbit]. *)

val ntests_rabbit : int
(** Number of tests in [Rabbit]: 26. *)

(** {2 Alphabit} *)

val alphabit : Unif01.gen -> float -> int -> int -> unit
(** [alphabit gen nb r s] applies the [Alphabit] battery of tests to the
   generator [gen] using at most [nb] bits for each test. The bits themselves
   are processed as blocks of 32 bits (unsigned integers). For each block of 32
   bits, the r most significant bits are dropped, and the test is applied on the
   s following bits. If one wants to test all bits of the stream, one should set
   r = 0 and s = 32. If one wants to test only 1 bit out of 32, one should set s
   = 1. See the description of the tests in {!alphabit_file}. *)

val alphabit_file : string -> float -> unit

(** [alphabit_file filename nb] applies the [Alphabit] battery of tests to the
   first [nb] bits (or less, if [nb] is too large) of the binary file
   [filename]. Unlike the {!alphabit} function above, for each test, the file is
   rewound and the test is applied to the bit stream starting at the beginning
   of the file. On a PC with an Athlon processor of clock speed 1733 MHz running
   under Linux, [Alphabit] takes about 4.2 seconds to test a file of 2²⁵ bits,
   and 2.3 minutes to test a file of 2³⁰ bits. [Alphabit] and [AlphabitFile]
   have been designed primarily to test hardware random bits generators. The
   four [multinomial_bits_over] tests should detect correlations between
   successive bits by applying a [serial_over] test to overlapping blocks of 2,
   4, 8 and 16 bits. The [hamming_*] tests should detect correlations between
   the successive bits of overlapping blocks of 16 and 32 bits, and the
   [random_walk] tests consider blocks of 64 and 320 bits.

   + {!Smultin.multinomial_bits_over} with L = 2.
   + {!Smultin.multinomial_bits_over} with L = 4.
   + {!Smultin.multinomial_bits_over} with L = 8.
   + {!Smultin.multinomial_bits_over} with L = 16.
   + {!Sstring.hamming_indep} with blocks of L = 16 bits.
   + {!Sstring.hamming_indep} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 32 bits.
   + {!Swalk.random_walk_1} with walks of length L = 64.
   + {!Swalk.random_walk_1} with walks of length L = 320. *)

val repeat_alphabit : Unif01.gen -> float -> int -> int -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Alphabit]. *)

val ntests_alphabit : int
(** Number of tests in [Alphabit]: 9. *)

(** {2 BlockAlphabit} *)

val block_alphabit : Unif01.gen -> float -> int -> int -> unit
val block_alphabit_file : string -> float -> unit

(** Apply the Alphabit battery of tests repeatedly to the generator [gen] or to
   the binary file [filename] after reordering the bits as described in the
   filter {!Unif01.create_bit_block_gen}. [Alphabit] will be applied for the
   different values of w ∈ \{1,2,4,8,16,32\}. If s < 32, only values of w≤s will
   be used. Each test uses at most [nb] bits. See the description of the tests
   in {!alphabit_file}. *)

val repeat_block_alphabit : Unif01.gen -> float -> int -> int -> int array -> int -> unit
(** Similar to {!repeat_small_crush} above but applied on [BlockAlphabit]. The
   parameter w is the one described in {!block_alphabit}. Restrictions: w ∈
   \{1,2,4,8,16,32\} and w≤s. *)

val ntests_block_alphabit : int
(** Number of tests in [BlockAlphabit]: 9. *)

(** {2 PseudoDIEHARD} *)

val pseudo_diehard : Unif01.gen -> unit

(** Applies the battery [PseudoDIEHARD], which implements most of the tests in
   the popular battery DIEHARD or, in some cases, close approximations to them.
   We do not recommend this battery as it is not very stringent (we do not know
   of any generator that passes the batteries [Crush] and [BigCrush], and fails
   [PseudoDIEHARD], while we have seen the converse for several defective
   generators). It is included here only for convenience to the user. The
   DIEHARD tests and the corresponding tests in [PseudoDIEHARD] are:

   + The Birthday Spacings test. This corresponds to {!Smarsa.birthday_spacings}
     with n = 512, d = 224, t = 1 and r = 0,1,2,3,4,5,6,7,8,9 successively. The
     test with each value of r is repeated 500 times and a chi-square test is
     then applied.
   + The Overlapping 5-Permutation test. This test is not implemented in TestU01.
   + The Binary Rank Tests for Matrices. This corresponds to {!Smarsa.matrix_rank}.
   + The Bitstream test. Closely related to {!Smulti.multinomial_bits_over} with
     Delta = −1, n = 221, L = 20.
   + The OPSO test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 1024, t = 2 and all values of r from 0 to 22.
   + The OQSO test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 32, t = 4 and all values of r from 0 to 27.
   + The DNA test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 4, t = 10 and all values of r from 0 to 30.
   + The Count-the-1's test is not implemented in TestU01. It is a 5-dimensional
     over-lapping version of {!Sstring.hamming_indep}.
   + The Parking Lot test is not implemented in TestU01.
   + The Minimum Distance test. Closely related to {!Snpair.close_pairs} with
     N = 100, n = 8000, t = 2, p = 2, m = 1.
   + The 3-D Spheres test. Closely related to {!Snpair.close_pairs} with N = 20,
     n = 4000, t = 3, p = 2, m = 1.
   + The Squeeze test. Closely related to {!Smarsa.savir_2}.
   + The Overlapping Sums test is not implemented in TestU01.
   + The Runs test. This corresponds to {!Sknuth.run}.
   + The Craps test is not implemented in TestU01. *)

val ntests_pseudo_diehard : int
(** Number of tests in [PseudoDIEHARD]: 15. *)

(** {2 NIST}

   The NIST (National Institute of Standards and Technology) of the U.S.
   federal government has proposed a statistical test suite for use in the
   evaluation of the randomness of bitstreams produced by cryptographic random
   number generators. The test parameters are not predetermined. The NIST tests
   and the equivalent tests in TestU01 are:

   + The Monobit test. This corresponds to {!Sstring.hamming_weight_2} with L = n.
   + The Frequency test within a Block. Corresponds to {!Sstring.hamming_weight_2}.
   + The Runs test. Is implemented as {!Sstring.run}.
   + The test for the Longest Run of Ones in a Block. Is implemented as the test {!Sstring.longest_head_run}.
   + The Binary Matrix rank test. Is implemented as {!Smarsa.matrix_rank}.
   + The Discrete Fourier Transform test. Is implemented as {!Sspectral.fourier_1}.
   + The Non-overlapping Template Matching test. Is implemented as the test {!Smarsa.cat_bits}.
   + The Overlapping Template Matching test. This test does not exist as such in TestU01, but a similar and more powerful test is {!Smultin.multinomial_bits_over}.
   + Maurer’s Universal Statistical test. This test is implemented as {!Svaria.appearance_spacings}.
   + The Lempel-Ziv Compression test. Is implemented as {!Scomp.lempel_ziv}.
   + The Linear Complexity test. Is implemented as part of {!Scomp.linear_comp}.
   + The Serial test. Corresponds to {!Smultin.multinomail_bits_over} with Delta = 1.
   + The Approximate Entropy test. Corresponds to {!Smultin.multinomial_bits_over} with Delta = 0, and to {!Sentrop.entropy_disc_over} or {!Sentrop.entropy_disc_over_2}.
   + The Cumulative Sums test. This test is closely related to the M statistic in {!Swalk.random_walk_1}.
   + The Random Excursions test. This test does not exist in TestU01, but closely related tests are in {!Swalk.random_walk_1}.
   + The Random Excursions Variant test. This test does not exist in TestU01, but a closely related test is based on the R statistic in {!Swalk.random_walk_1}. *)

(** {2 FIPS-140-2} *)

val fips_140_2 : Unif01.gen -> unit
val fips_140_2_file : string -> unit

(** These functions apply the four tests described in the NIST document {i FIPS
   PUB 140-2, Security Requirements for Cryptographic Modules}, page 35, with
   exactly the same parameters (see the WEB page at
   {{: http://csrc.nist.gov/rng/rng6_3.html}
   http://csrc.nist.gov/rng/rng6_3.html}). They report the values of the test
   statistics and their p-values (except for the runs test) and indicate which
   values fall outside theintervals specified by FIPS-140-2. The first function
   applies the tests on a generator [gen], and the second applies them on the
   file of bits [filename]. First, 20000 bits are generated and putin an array,
   then the tests are applied upon these. The tests applied are:

   + The Monobit test. This corresponds to {!Smultin.multinomal_bits} with s = 32, L = 1, n = 20000.
   + The “poker” test, which is in fact equivalent to {!Smultin.multinomial_bits} with s = 32, L = 4, n = 5000.
   + The Runs test, which is related to {!Sstring.run}.
   + The test for the Longest Run of Ones in a Block, which is implemented as {!Sstring.longest_head_run}. *)

val ntests_fips_140_2 : int
(** Number of tests in [FIPS-140-2]: 4. *)
