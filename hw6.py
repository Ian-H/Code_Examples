#ian heath - iheath - section D

#This python code is provided as an example of my
#coding style.

import copy

class Vehicle(object):
    def __init__(self):
        pass
    def __repr__(self):
        return "Vehicle()"
    def getWheels(self):
        raise NotImplementedError
        
class Car(Vehicle):
    wheels = 4
    def __init__(self,licenseNum,mpg):
        super(Car, self).__init__()
        self.licenseNum = licenseNum
        self.mpg = mpg

    def __repr__(self):
        return "Car('%s',%d)"%(self.licenseNum,self.mpg)

    @classmethod
    def getWheels(self):
        return Car.wheels

class Bike(Vehicle):
    wheels = 2
    def __init__(self):
        super(Bike, self).__init__()
    def __repr__(self):
        return "Bike()"
    def getWheels(self):
        return Bike.wheels
        
class Person(object):
    def __init__(self,name,parent=None):
        self.name = name
        self.parent = parent
        self.children = []

    def addChild(self,child):
        child = Person(child, self)
        self.children.append(child)
        return child

    def getChildren(self):
        return self.children

    def getSiblings(self):
        if self.parent == None: return []
        siblings = copy.copy(self.parent.getChildren())
        siblings.remove(self)
        return siblings

    def getCousins(self):
        cousinList = []
        if self.parent == None: return []
        myParent = self.parent
        for sibling in myParent.getSiblings():
            cousinList+=(sibling.getChildren())
        return cousinList

    def getOffspring(self):
        if self.getChildren() == None:
            return []
        else:
            offspring = []
            for child in self.getChildren():
                offspring+=[child]
                offspring+=child.getOffspring()
            return offspring

    def __repr__(self):
        return self.name
                
class Foo(object):
    currPair = 0
    def __init__(self,x,y=1):
        self.x = x
        self.y = y
        self.callsToGetSumCount = 0
        self.currPair = 0

    def __eq__(self,other):
        return isinstance(other,Foo) and (self.x==other.x) and (self.y==other.y)

    def callsToGetSum(self):
        return self.callsToGetSumCount

    def getSum(self):
        self.callsToGetSumCount+=1
        return self.x+self.y
    
    def __repr__(self):
        return "Foo(%d,%d)"%(self.x,self.y)

    def addValues(self,other):
        self.x+=other.x
        self.y+=other.y

    def __add__(self,other):
        tempX = self.x+other.x
        tempY = self.y+other.y
        newFoo = Foo(tempX,tempY)
        return newFoo
    
    def __hash__(self):
        # replace hashables tuple with instance data attributes for your own class
        hashables = (self.x, self.y)
        # Then just use the code below unmodified.
        # It is based on Bernstein's hash function, which is
        # simple enough but works well enough
        result = 0
        for value in hashables:
            result = 33*result + hash(value)
        return hash(result)

    @classmethod
    def nextPair(self):
        f1 = Foo(Foo.currPair,Foo.currPair)
        Foo.currPair+=1
        return f1
