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
# boardTwo = Board()
# boardTwo.setBackplane(plane)
# boardOne.connectTo(1, board1, 1) #boardOne output 1 to board1 input 1
# boardOne.connectTo(1, board1, 2) #boardOne output 1 to board1 input 2
# boardOne.connectTo(2, board1, 1) #boardOne output 2 to board1 input 1
## board one: 
#
# out 0: i0 + i1
# out 1: i1 + i2
# out 2: io + i2
#
# set up the board
boardOne.setSigBlock(SigBlock.fork(), 0, 0)
boardOne.setSigBlock(SigBlock.fork(), 0, 1)
boardOne.setSigBlock(SigBlock.fork(), 0, 2)
boardOne.connectToSigBlock(0, 0 ,0) # connect input 0 on the board to input 0 on the sigblock in position 0
boardOne.connectToSigBlock(1, 1, 0) # connect input 1 on the board to input 0 on the sigblock in position 1
boardOne.connectToSigBlock(2, 2, 0) # connect input 2 on the board to input 0 on the sigblock in position 2
boardOne.setSigBlock(SigBlock.add(), 1, 0)
boardOne.setSigBlock(SigBlock.add(), 1, 1)
boardOne.setSigBlock(SigBlock.add(), 1, 2)
boardOne.setSigBlock(SigBlock.thru3(), 2, 1)

# connect the sigblocks
# connect the forked signals to the and blocks
boardOne.interconnectSB(0,0,0, 1,0,0) # connect output 0 on sigblock at 0,0 (i0) to input 0 on sigblock at 1,0
boardOne.interconnectSB(0,0,1, 1,2,0) # connect output 1 on sigblock at 0,0 (i0) to input 0 on sigblock at 1,2
boardOne.interconnectSB(0,1,0, 1,0,1) # connect output 0 on sigblock at 0,1 (i1) to input 1 on sigblock at 1,0
boardOne.interconnectSB(0,1,1, 1,1,0) # connect output 1 on sigblock at 0,1 (i1) to input 0 on sigblock at 1,1
boardOne.interconnectSB(0,2,0, 1,1,1) # connect output 0 on sigblock at 0,2 (i2) to input 1 on sigblock at 1,1
boardOne.interconnectSB(0,2,1, 1,2,1) # connect output 1 on sigblock at 0,2 (i2) to input 1 on sigblock at 1,0
# connect the and gates to the passthrough
boardOne.interconnectSB(1,0,0, 2,1,0) # connect output 0 on sigblock at 1,0 (i0+i1) to input 0 on sigblock at 2,1
boardOne.interconnectSB(1,1,0, 2,1,1) # connect output 0 on sigblock at 1,1 (i1+i2) to input 1 on sigblock at 2,1
boardOne.interconnectSB(1,2,0, 2,1,2) # connect output 0 on sigblock at 1,2 (i0+i2) to input 2 on sigblock at 2,1

boardOne.connectFromSigBlock(0, 1 ,0) # connect output 0 on the board to output 0 on the sigblock in position 0
boardOne.connectFromSigBlock(1, 1, 1) # connect output 1 on the board to output 1 on the sigblock in position 1
boardOne.connectFromSigBlock(2, 1, 2) # connect output 2 on the board to output 2 on the sigblock in position 2


boardOne.connectSensor(0,s0)
boardOne.connectSensor(1,s1)
boardOne.connectSensor(2,s2)

boardOne.connectActuator(0,a0)
boardOne.connectActuator(1,a1)
boardOne.connectActuator(2,a2)

a0.alert()
a1.alert()
a2.alert()

plane.tick()

a0.alert()
a1.alert()
a2.alert()


#  ************************************
#  *                                   *
#  *  /-0       /--0     1---\         *
#  1-/    add 1/   1 pass1   |         1
#  * /--1        /-2     2-\ |         *
#  * |           |         | |         *
#  * |        1-/          | \-1       *
#  1/ /-2 fork             | |  mul 1--1
#  *  |       2-           \---1       *
#  *  |                    | |         *
#  *  |                    | \-2       *
#  2--/                    |    and 1--2
#  *                       \-- 4       *
#  *                                   *
#  ************************************

#  ************************************
#  *                                  *
#  * /-1                              *
#  1-    add 1                        1
#  * /-1                              *
#  *|                                 *
#  *|                                 *
#  1/                                 1
#  *                                  *
#  *                                  *
#  *                                  *
#  2                                  2
#  *                                  *
#  *                                  *
#  ************************************
