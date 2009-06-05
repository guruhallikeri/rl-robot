package edu.torax.reinforcement.tests.gutils
import org.scalatest.FunSuite
import reinforcement.gutils.Polygon
import reinforcement.gutils.Vector
import reinforcement.robot._

class PolygonTests extends FunSuite {
	test ("Obstacle collides") {
	  val p1 = List(Vector(11.94966889149483, 16.338625191499602),
                 Vector(6.615677413440637, 17.54729314556458),
                 Vector(7.237805118724304, 21.159654540893847),
                 Vector(8.077584179749271, 21.821153712908696))
    val p2 = List(Vector(5.782274468857947, 17.64518355805145), 
                  Vector(6.442347283836307, 18.39638518140336), 
                  Vector(7.193548907188216, 17.736312366424997), 
                  Vector(6.533476092209856, 16.985110743073086))
    assert (Polygon.distanceBetween(p1,p2) < 2.0*RobotSessionSettings.Defaults.maxDistanceToGoal)
  }
 
	test ("Obstacle collision") {
	  val p1 = List(Vector(25.20752726466157, 16.617995332570885),
                 Vector(20.29647704569056, 13.781720143685085),
                 Vector(17.99146459047844, 16.387881288805293),
                 Vector(17.903995912102086, 16.945372461295126),
                 Vector(22.761197017620688, 20.723677811103062),
                 Vector(23.33873396594558, 20.470430976964575))
    val p2 = List(Vector(22.880932871991824, 16.02281603629156), 
                 Vector(22.083758592533265, 16.626565295644916), 
                 Vector(22.687507851886622, 17.423739575103475), 
                 Vector(23.48468213134518, 16.81999031575012))
    assert (Polygon.distanceBetween(p1,p2) < 2.0*RobotSessionSettings.Defaults.maxDistanceToGoal)
	}
 
	test ("more tests") {
	  val p1 = List(Vector(25.327341261713702, 6.338310256880962), 
                 Vector(26.014938873219545, 5.6122183035778965), 
                 Vector(25.288846919916477, 4.924620692072055), 
                 Vector(24.601249308410633, 5.65071264537512))
    val p2 = List(
      List(Vector(27.272758008619896, 4.058351638352402),
           Vector(24.232639261868336, 2.694978901497641),
           Vector(21.890977459447516, 3.136708005837332),
           Vector(20.600772076094568, 10.902586291498661),
           Vector(28.52407987894868, 8.744662242775165)),
      List(Vector(7.393283643127532, 20.138081875533757),
           Vector(4.961062017282158, 18.4563271679821),
           Vector(2.5486531706257494, 21.391672014144376),
           Vector(3.4267049957758506, 22.90415988336992),
           Vector(5.093888442920403, 23.47602325024807)),
      List(Vector(35.756286001898346, 9.836444828477614),
           Vector(34.131896568122045, 8.865680693496408),
           Vector(33.661177246875816, 8.777296441290655),
           Vector(30.061221156527104, 11.04624646592334),
           Vector(29.95389916536187, 12.868463116146204),
           Vector(32.40018196404151, 15.430560349825285)),
      List(Vector(9.009100309081864, 33.71669991641786),
           Vector(7.949781795494041, 31.466294737078897),
           Vector(7.437091990537289, 31.08169347587462),
           Vector(2.6876349767109993, 32.68193781471441),
           Vector(2.544862966897289, 34.62478927023034),
           Vector(4.947577636930336, 37.06306140722391),
           Vector(8.760806469278734, 35.14401484681581))
    )
    var cnt = 0
    for (pp <- p2) {
      if (Polygon.distanceBetween(p1, pp) < RobotSessionSettings.Defaults.maxDistanceToGoal)
        cnt += 1
    }
    expect (1) { cnt }
	}
}
