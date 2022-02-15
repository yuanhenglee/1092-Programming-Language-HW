class Point:
	# hidden data: instance variables
	# class variable
	count = 0;
   	# constructor: class method
	def __init__(self, x, y):
		self.x_ = x
		self.y_ = y
		Point.count+=1
   	# instance methods
	def getX(self):
		return self.x_
	def getY(self):
		return self.y_
	def move(self, dx, dy):
		self.x_ = self.x_ + dx
		self.y_ = self.y_ + dy
	def __str__(self):
		return "A point at "+str(self.getX())+","+str(self.getY())
	def draw(self):
		print(self)

pt1 = Point(3, 5)
pt1.draw()
print(Point.count)
pt1.move(1, 1)
pt1.draw()
pt2 = Point(2, 2)
print(Point.count)