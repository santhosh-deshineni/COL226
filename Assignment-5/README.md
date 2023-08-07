## README for COL226 Assignment-5

### Design Decisions and Implementation Details

1. `subseq.P` contains the main predicate `subsequence` which takes two arguments with the first one being the subsequence of the second one. The base case is for an empty list which is always a subsequence of any list followed by pattern matching for equal heads and finally the last case if they do not match.

2. `triplicates.P` contains the main predicate `has_no_triplicates` which takes one argument which is the list being checked. The `one_or_less` predicate checks if there are one or lesser occurrences of an element in a list and `not_in_list` is true if the element A is not arithmetically equal to any of the elements present in a list.

3. `arith.P` contains the main predicate `arith` which takes one argument that is the list to which we insert the arithmetic operators and prints out the various possible combinations. Please note that for a +ve integer input (without an explicit '+' say for example `1 in [1,2,3]`) a space gets inserted if there is a '-' sign to the left of the integer in the output. Also, if you provide the integer input with sign(either '+' or '-' say for example `-1 in [-1,2,3]` or `+1 in [+1,2,3]`) then a space gets inserted before this integer irrespective of the sign present to the left of it in the output. 

4. `ABCD.P` contains the main predicate `abcd` which takes no arguments but prints out the possible cases of the various crossings along with the paddler. `get_remain` is a predicate which gets the remaining part of the list after a certain element. `nc2` is a predicate for selecting 2 elements from a list. `in_list` checks if a certain element is present in a list and `sub_list` gives true if all the elements of the 1st list are present in the 2nd list. `paddle` gives all the possibilities of the paddler given a list of the people in the canoe. Finally, the output shows the people in the canoe and the paddler for each of the crossings.

### Acknowledgements
- [Prolog Tutorial](https://www.lix.polytechnique.fr/~liberti/public/computing/prog/prolog/prolog-tutorial.html) for prolog documentation