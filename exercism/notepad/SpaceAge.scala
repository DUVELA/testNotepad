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
    def onEarth(x : Double) = x / ddt(onSecEarthTime)("Earth")
    def onMercury(x: Double) = x / ddt(onSecEarthTime)("Mercury")
    def onVenus(x: Double) = x / ddt(onSecEarthTime)("Venus")
    def onMars(x: Double) = x / ddt(onSecEarthTime)("Mars")
    def onJupiter(x: Double) = x / ddt(onSecEarthTime)("Jupiter")
    def onSaturn(x: Double) = x / ddt(onSecEarthTime)("Saturn")
    def onUranus(x: Double) = x / ddt(onSecEarthTime)("Uranus")
    def onNeptune(x: Double) = x / ddt(onSecEarthTime)("Neptune")
    /* ====== back story end */

    def ddt(acc: Double)(y: String) = onSecEarthTime * howOldRU(y)
    // y = () => ???
}
