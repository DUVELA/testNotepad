object SpaceAge {
    
    val onSecEarthTime: Double = 31557600.0
   
    val howOldRU = Map(
        "Earth" -> 1.0,
        "Mercury" -> 0.2408467,
        "Venus" -> 0.61519726,
        "Mars" -> 1.8808158,
        "Jupiter" -> 11.862615,
        "Saturn" -> 29.447498,
        "Uranus" -> 84.016846,
        "Neptune" -> 164.79132
        )

    /* ====== back story()()() start */
    def onEarth(x : Double) = x / ddt()("Earth")
    def onMercury(x: Double) = x / ddt()("Mercury")
    def onVenus(x: Double) = x / ddt()("Venus")
    def onMars(x: Double) = x / ddt()("Mars")
    def onJupiter(x: Double) = x / ddt()("Jupiter")
    def onSaturn(x: Double) = x / ddt()("Saturn")
    def onUranus(x: Double) = x / ddt()("Uranus")
    def onNeptune(x: Double) = x / ddt()("Neptune")
    /* ====== back story end */

    def ddt(acc: Double = onSecEarthTime)(y: String) = acc * howOldRU(y)
    // y = () => ???
}
