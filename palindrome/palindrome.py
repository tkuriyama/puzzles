from __future__ import division
import sys

# Helpers

def add_delim(string, char):
    """Add delimeter char to bookend and separate string chars."""
    text = char + char.join(list(string.lower())) + char
    return text, len(text)

def reset_pos(left, center, right, adj_right):
    """If ctr unmoved, set to adj_right. Mirror left around ctr."""
    if (left + right) / 2 == center:
        center = adj_right
    left = center - (right - center)
    return left, center, right

def symmetric(left, left_edge, right, right_edge, text):
    """Check for symmetry, i.e. left and right are in bounds and chars match."""
    return (left >= left_edge and
            right <= right_edge and
            text[left] == text[right])

# Algorithm

def expand_linear(center, right, adj_right, len_sps):
    """Expand as much as possible using 3-case "mirror" principle of algo."""

    for i in xrange(1, right - center):
        dist_to_edge = adj_right - (center + i)

        if len_sps[center - i] < dist_to_edge:
            len_sps[center + i] = len_sps[center - i]
        elif len_sps[center - i] > dist_to_edge:
            len_sps[center + i] = dist_to_edge
        else:
            len_sps[center + i] = dist_to_edge
            center += i
            break

    return center, len_sps

def find_sps(string, delim):
    """Find all SPs for a given string.
    Add delimeter chars to the original string to simplify indexing.

    Args
        string: string in which to search for subpalindromes
        delim: char to use as algorithm delimeter, should not occur in string
    Returns
        List of the length of the subpalindrome at each location in the
        delimited string.
    """
    if string == '': return [0]

    text, len_text = add_delim(string, delim)
    len_sps = [0] * len_text

    left, center, right = 1, 1, 1
    left_edge, right_edge = 1, len(text) - 2

    while right <= right_edge:
        # expand naively around current center; single char is palindrome
        while symmetric(left, left_edge, right, right_edge, text):
            if text[right] != delim:
                len_sps[center] += 1 if left == right else 2
            left, right = left - 1, right + 1

        # find adjusted right index that always falls on a delimeter
        adj_right = right - 1 if len_sps[center] > 0 and left > 0 else right

        # expand using linear algo and reset position indices
        center, len_sps = expand_linear(center, right, adj_right, len_sps)
        left, center, right = reset_pos(left, center, right, adj_right)

    return len_sps

def longest_sp(string, delim='|'):
    """Find longest SP within given string.
    Returns
        tuple of ints (start, end), which return the longest SP
        when used to slice string[start: end]
    """
    len_sps = find_sps(string, delim)
    longest = max(len_sps)

    # account for presence of delimeters; int takes floor of float
    ctr_ind = int(len_sps.index(longest) / 2)
    half_word = int(longest / 2)

    start, end = ctr_ind - half_word, ctr_ind + half_word
    end += 0 if longest % 2 == 0 else 1

    return start, end

# Test

def test_add_delim():
    """Test add_delim() for some corner cases."""
    assert ('||', 2) == add_delim('', '|'), 'empty case fails'
    assert ('|a|', 3) == add_delim('a', '|'), 'single char case fails'
    assert (',a,b,', 5) == add_delim('ab', ','), 'two char case fails'
    return True

def test_reset_pos():
    """Test various cases for adjust_pos()."""
    assert (19, 20, 21) == reset_pos(1, 11, 21, 20)
    assert (3, 5, 7) == reset_pos(1, 5, 7, 6)
    assert (19, 19, 19) == reset_pos(3, 11, 19, 19)
    assert (11, 15, 19) == reset_pos(3, 15, 19, 19)
    return True

def test_longest_sp():
    """Test longest_SP() with different inputs."""
    L = longest_sp
    assert L('babcbabcbaccba') == (1, 10)
    assert L('racecar') == (0, 7)
    assert L('Racecar') == (0, 7)
    assert L('RacecarX') == (0, 7)
    assert L('Race carrrs') == (7, 10)
    assert L('') == (0, 0)
    assert L('something rac e car going') == (8, 21)
    assert L('xxxxx') == (0, 5)
    assert L('Mad am I ma dam.') == (0, 15)
    assert L('amanaplanacanalpanama') == (0, 21)
    assert L('aabcd') == (0, 2)
    return True

def unit_tests():
    """Unit tests."""
    test_add_delim()
    test_reset_pos()
    test_longest_sp()
    return True

# Main

def main(string):
    """Return longest SP."""
    start, end = longest_sp(string)
    pal = string[start:end]
    print pal
    return pal

if __name__ == '__main__':
    if len(sys.argv) == 2:
        unit_tests()
        main(sys.argv[1])
    else:
        print 'Call with single argument, e.g. python palindrome.py racecar'
