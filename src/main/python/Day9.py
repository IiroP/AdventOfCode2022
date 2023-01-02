import numpy as np

class Day9:

	directions = list()
	h_pos = (0,0)
	t_pos = (0,0)
	visited = [(0, 0)]
	visit9 = [(0,0)]
	tail = [(0,0)] * 9

	def parseFile(self):
		with open("../../../input/day9_input") as file:
			for line in file.readlines():
				parts = line.strip().split(" ")
				self.directions.append((parts[0], int(parts[1])))


	def run(self):
		def updateTail():
			def posDiff(pos1, pos2):
				xDiff = pos1[0] - pos2[0] # Positive means head is on the right
				yDiff = pos1[1] - pos2[1] # Positive means head is on top
				return (xDiff, yDiff)

			for part in range(len(self.tail)):
				if part == 0:
					xDiff, yDiff = posDiff(self.h_pos, self.tail[0])
				else:
					xDiff, yDiff = posDiff(self.tail[part-1], self.tail[part])
				
				if abs(xDiff) <= 1 and abs(yDiff) <= 1:
					continue

				if xDiff > 0:
					x = 1
				elif xDiff < 0:
					x = -1
				else:
					x = 0
				
				if yDiff > 0:
					y = 1
				elif yDiff < 0:
					y = -1
				else:
					y = 0

				self.tail[part] = tuple(np.add(self.tail[part], (x, y)))

			self.t_pos = self.tail[0]
			self.visited.append(self.t_pos)
			self.visit9.append(self.tail[-1])

		for line in self.directions:
			command = line[0]
			for i in range(line[1]): # run x times
				if command == "U":
					self.h_pos = tuple(np.add(self.h_pos, (0, 1)))
				elif command == "D":
					self.h_pos = tuple(np.add(self.h_pos, (0, -1)))
				elif command == "R":
					self.h_pos = tuple(np.add(self.h_pos, (1, 0)))
				elif command == "L":
					self.h_pos = tuple(np.add(self.h_pos, (-1, 0)))
				
				updateTail()

	def countPositions(self):
		#print(self.visited)
		return len(set(self.visited))

	def task2_count(self):
		#print(self.visit9)
		return len(set(self.visit9))

def day9_start():
	day = Day9()
	day.parseFile()
	#print(day.directions)
	day.run()
	print(day.countPositions())
	print(day.task2_count())

day9_start()
