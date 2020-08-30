//https://wrobstory.gitbooks.io/python-to-scala/content/classes/README.html

class Automobil(var whlees: Int = 4, var engine: Int = 1, var lights: Int = 2){
        def total_parts() = whlees + engine + lights
        def remove_wheels(count: Int) = {
            if (whlees - count < 0)
                throw new IllegalArgumentException("Automobile cannot have fewer than 0 wheels!")
            else 
                whlees = whlees - count
                println(s"The automobile now has $whlees wheels!")
        }
        def add_wheels(count: Int) = {
            whlees = whlees + count
            println(s"The automobile now has $whlees wheels!")
        }
    }

/* Python start
class Automobile(object):

    def __init__(self, wheels=4, engine=1, lights=2):
        self.wheels = wheels
        self.engine = engine
        self.lights = lights

    def total_parts(self):
        return self.wheels + self.engine + self.lights

    def remove_wheels(self, count):
        if (self.wheels - count) < 0:
            raise ValueError('Automobile cannot have fewer than 0 wheels!')
        else:
            self.wheels = self.wheels - count
            print('The automobile now has {} wheels!').format(self.wheels)
Python end*/
/* cmd start
scala> val car = new Automobil
val car: Automobil = Automobil@6ccf6df3

scala> car.whlees
val res2: Int = 4

car.remove_wheels(5)
java.lang.IllegalArgumentException: Automobile cannot have fewer than 0 wheels!
  at Automobil.remove_wheels(<console>:5)
  ... 40 elided
  
scala> car.remove_wheels(4)
The automobile now has 0 wheels!

scala> car.total_parts
val res17: Int = 3

scala> car.whlees
val res19: Int = 0

scala> car.add_wheels(4)
The automobile now has 4 wheels!

scala> car.total_parts
val res24: Int = 7

scala> car.whlees 
val res26: Int = 4
cmd end */
