package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {

  val buildImg3x4 = {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16
    src
  }
  
  val blur3x4R1 = {
    val dest = new Img(3, 4)
    dest(0, 0) = 2; dest(1, 0) = 2; dest(2, 0) = 3
    dest(0, 1) = 3; dest(1, 1) = 4; dest(2, 1) = 4
    dest(0, 2) = 13; dest(1, 2) = 12; dest(2, 2) = 8
    dest(0, 3) = 18; dest(1, 3) = 16; dest(2, 3) = 10
    dest
  }

  val blur3x4R7 = {
    val dest = new Img(3, 4)
    dest(0, 0) = 9; dest(1, 0) = 9; dest(2, 0) = 9 
    dest(0, 1) = 9; dest(1, 1) = 9; dest(2, 1) = 9 
    dest(0, 2) = 9; dest(1, 2) = 9; dest(2, 2) = 9 
    dest(0, 3) = 9; dest(1, 3) = 9; dest(2, 3) = 9 
    dest
  }


  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel of a 3x4 image with radius 1") {
    val src = buildImg3x4
    val result = blur3x4R1
    
    assert(boxBlurKernel(src, 1, 2, 1) === result(1, 2),
      s"(boxBlurKernel(1, 2, 1) should be ${result(1, 2)}, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel should return the correct value on an interior pixel of a 3x4 image with radius 7") {
    val src = buildImg3x4
    val result = blur3x4R7
    
    assert(boxBlurKernel(src, 1, 2, 7) === result(1, 2),
      s"(boxBlurKernel(1, 2, 1) should be ${result(1, 2)}, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 7)})")
  }


  test("boxblurkernel should return the correct value on an interior pixel of a 4x3 image with radius 2 ") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    assert(boxBlurKernel(src, 0, 0, 2) === 4,
      s"(boxBlurKernel(0, 0, 2) should be 4, " +
        s"but it's ${boxBlurKernel(src, 0, 0, 2)})")

   }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }

  test("VerticalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image ") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2;
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5;
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8;

    VerticalBoxBlur.blur(src, dst, 0, 3, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

  test("VerticalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image ") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2;
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5;
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8;

    VerticalBoxBlur.parBlur(src, dst, 4, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image ") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2;
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5;
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8;

    HorizontalBoxBlur.parBlur(src, dst, 4, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

}
