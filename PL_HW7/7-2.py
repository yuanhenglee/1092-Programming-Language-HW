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

class ColorPoint(Point):
	# constructor
	def __init__(self, x, y, c):
		super().__init__(x, y)
		self.color = c
	def getColor(self):
		return self.color
	def setColor(self, c):
		self.color = c
	# method overriding
	def __str__(self):
		return super().__str__()+" with color:"+str(self.getColor())


cpt = ColorPoint(3, 5, "RED")

cpt.draw()

cpt.move(1, 1)

cpt.draw()
