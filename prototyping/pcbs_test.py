from pcbs import SigBlock,Board,Backplane,ConstSensor,AlertActuator
# Goal is to set up a board, connect it to sensors and actuators.
plane = Backplane()
boardOne = Board()

s0 = ConstSensor(128)
s1 = ConstSensor(67)
s2 = ConstSensor(22)

a0 = AlertActuator()
a1 = AlertActuator()
a2 = AlertActuator()

plane.setBoard(boardOne,0)

## board one: 
#
# out 0: i0 + i1
# out 1: i1 + i2
# out 2: io + i2
#
# set up the board
# because each signal gets used in two operations, we have to fork all the signals right off the bat:
boardOne.setSigBlock(SigBlock.fork(), 0, 0) # first col, first row, put in a fork block
boardOne.setSigBlock(SigBlock.fork(), 0, 1)
boardOne.setSigBlock(SigBlock.fork(), 0, 2)
# Next we'll connect the inputs to the board to those fork signal blocks
boardOne.connectToSigBlock(0, 0 ,0) # connect input 0 on the board to input 0 on the sigblock in position 0
boardOne.connectToSigBlock(1, 1, 0) # connect input 1 on the board to input 0 on the sigblock in position 1
boardOne.connectToSigBlock(2, 2, 0) # connect input 2 on the board to input 0 on the sigblock in position 2
# Now we'll add the remaining signal blocks:
# first put in our three adder blocks
boardOne.setSigBlock(SigBlock.add(), 1, 0)
boardOne.setSigBlock(SigBlock.add(), 1, 1)
boardOne.setSigBlock(SigBlock.add(), 1, 2)
# then, because only the 3rd column can connect to outputs, put in a passthrough block in the third column.
boardOne.setSigBlock(SigBlock.thru3(), 2, 1)

# connect the sigblocks
# connect the forked signals to the and blocks
# connecting input 1
boardOne.interconnectSB(0,0,0, 1,0,0) # connect output 0 on sigblock at 0,0 (i0) to input 0 on sigblock at 1,0
boardOne.interconnectSB(0,0,1, 1,2,0) # connect output 1 on sigblock at 0,0 (i0) to input 0 on sigblock at 1,2
# connecting input 2
boardOne.interconnectSB(0,1,0, 1,0,1) # connect output 0 on sigblock at 0,1 (i1) to input 1 on sigblock at 1,0
boardOne.interconnectSB(0,1,1, 1,1,0) # connect output 1 on sigblock at 0,1 (i1) to input 0 on sigblock at 1,1
# connecting input 3
boardOne.interconnectSB(0,2,0, 1,1,1) # connect output 0 on sigblock at 0,2 (i2) to input 1 on sigblock at 1,1
boardOne.interconnectSB(0,2,1, 1,2,1) # connect output 1 on sigblock at 0,2 (i2) to input 1 on sigblock at 1,0
# connect the and gates to the passthrough
boardOne.interconnectSB(1,0,0, 2,1,0) # connect output 0 on sigblock at 1,0 (i0+i1) to input 0 on sigblock at 2,1
boardOne.interconnectSB(1,1,0, 2,1,1) # connect output 0 on sigblock at 1,1 (i1+i2) to input 1 on sigblock at 2,1
boardOne.interconnectSB(1,2,0, 2,1,2) # connect output 0 on sigblock at 1,2 (i0+i2) to input 2 on sigblock at 2,1
# connect the outputs from the passthrough to the board outputs
boardOne.connectFromSigBlock(0, 1 ,0) # connect output 0 on the board to output 0 on the sigblock in position 0
boardOne.connectFromSigBlock(1, 1, 1) # connect output 1 on the board to output 1 on the sigblock in position 1
boardOne.connectFromSigBlock(2, 1, 2) # connect output 2 on the board to output 2 on the sigblock in position 2

# attach the sensors
boardOne.connectSensor(0,s0)
boardOne.connectSensor(1,s1)
boardOne.connectSensor(2,s2)

# and the actuators
boardOne.connectActuator(0,a0)
boardOne.connectActuator(1,a1)
boardOne.connectActuator(2,a2)

# alert the state of the actuators before we do anything
a0.alert()
a1.alert()
a2.alert()

# tick the clock forward
plane.tick()

# and alert the state after everything has been ticked.
a0.alert()
a1.alert()
a2.alert()
