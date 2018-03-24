# Advent of Code: Day 4

"""A new system policy has been put in place that requires all accounts to 
use a passphrase instead of simply a password. A passphrase consists of a 
series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

"""
def valid(filename):
	f = open(filename, 'r')
	lines = f.readlines()
	f.close()
	
	result = 0
	for line in lines:
		split = line.rstrip().split(' ')
		if len(split) == len(set(split)):
			result += 1		
			
	return result
	

"""For added security, yet another system policy has been put in place. 
Now, a valid passphrase must contain no two words that are anagrams of 
each other - that is, a passphrase is invalid if any word's letters can 
be rearranged to form any other word in the passphrase.

"""		
def valid_anagram(filename):
	f = open(filename, 'r')
	lines = f.readlines()
	f.close()
	
	result = len(lines)
	for line in lines:
		split = line.rstrip().split(' ')
		split = [sorted(s) for s in split]
		for word in split:
			if split.count(word) > 1:
				result -= 1
				break		
			
	return result	
	
	
if __name__ == '__main__':
	print(valid('day4-input.txt'))
	print(valid_anagram('day4-input.txt'))