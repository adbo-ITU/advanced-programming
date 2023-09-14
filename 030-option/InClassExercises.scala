package adbo.inclass030;

enum BinaryBit:
  case Zero
  case One

enum BinaryNumber:
  case Bit(bit: BinaryBit)
  case Digit(digit: Bit, tail: BinaryNumber)
