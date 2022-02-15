def func():
    x = 10
    def getX():
        return x
    def setX(n):
        nonlocal x
        x = n
    return (getX, setX)

getX, setX = func()
print( getX() )
print( setX(20) )
print( getX() ) 
