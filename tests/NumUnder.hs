module NumUnder where

main :: IO ()
main = do
  print 1_000_000
  print 1__000000

  print 0.062_5
  print 0.062__5

  print 3_0e1
  print 3__0e1
  print 4_e+1
  print 4__e+1
  print 4e+0_1
  print 4e+0__1
  print 5_e-1
  print 5__e-1
  print 5e-0_1
  print 5e-0__1
  print 6_e1
  print 6__e1
  print 6e0_1
  print 6e0__1

  print 7_7.2_5
  print 7__7.2__5

  print 8_0.2_5_e0_1
  print 8__0.2__5e0__1

  print 0xffff
  print 0xff_ff
  print 0xff__ff
  print 0x_ffff
  print 0x__ffff

  print 0b1e10
  print 0b1p2
  print 0o7e3
  print 0o5p2
  print 0x1e1
  print 0x0p0
  print 0xffp10

  print 0b0.1
  print 0o0.1
  print 0o0.7
  print 0x0.1
  print 0x0.f
