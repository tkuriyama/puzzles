import palindrome

def main():
    """Find longest subpalindrome in War and Peace."""

    with open('war_and_peace_gutenberg.txt', 'r') as f:
        text = f.read()

    assert palindrome.main(text) == 'ton did not'
    return True

if __name__ == '__main__':
    main()