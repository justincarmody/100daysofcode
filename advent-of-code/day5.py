# Advent of Code: Day 5

def jumper(filename):
	f = open(filename)
	instructions = [int(i) for i in f.readlines()]
	f.close()
	
	cursor = 0
	jumps = 0
	while cursor < len(instructions):
		jump = instructions[cursor]
		dest = cursor + jump 
		
		instructions[cursor] = instructions[cursor] + 1
		
		cursor = dest		
		jumps += 1
	
	return jumps
	
	
def jumper_two(filename):
	f = open(filename)
	instructions = [int(i) for i in f.readlines()]
	f.close()
	
	cursor = 0
	jumps = 0
	while cursor < len(instructions):
		jump = instructions[cursor]
		dest = cursor + jump 
		
		if instructions[cursor] < 3:
			instructions[cursor] = instructions[cursor] + 1
		else:
			instructions[cursor] = instructions[cursor] - 1
		
		cursor = dest		
		jumps += 1
	
	return jumps
	
	
	
if __name__ == '__main__':
	print(jumper('day5-input.txt'))
	print(jumper_two('day5-input.txt'))