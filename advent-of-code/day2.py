# Advent of Code: Day 2

"""For each row, determine the difference 
between the largest value and the smallest value; 
the checksum is the sum of all of these differences.
"""
def checksum(filename):
	f = open(filename, 'r')
	rows = f.readlines()
	f.close()
	
	diffs = []
	
	for row in rows:
		nums = row.split('\t')
		nums = [int(i) for i in nums]
		diff = max(nums) - min(nums)
		diffs.append(diff)
		
	return sum(diffs)
	
	
	
def even_gcd(filename):
	f = open(filename, 'r')
	rows = f.readlines()
	f.close()
	
	results = []
	
	for row in rows:
		nums = [int(i) for i in row.split('\t')]
		
		for num in nums:
			store = [i for i in nums if i % num == 0 & i != num]
			
			if len(store) > 1:
				print(store)
				result = max(store)/min(store)
				results.append(result)
			
	return sum(results)
	
		
	
if __name__ == '__main__':
	print(checksum('day2-input.txt'))
	print(even_gcd('day2-input.txt'))