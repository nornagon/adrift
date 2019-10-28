# Computation model for adrift:
# Basic unit of compute is a "signal"
# signal is a value between 0 and 255.  It can represent an analog value (voltage, temperature, etc)
# in conidering it an analog value, adding two signals can saturate the channel.  The value does not 'roll over' like a digital value.
# similarly, subtracting can leave the channel empty - no 'reverse roll over' or negative values are possible.
# multiplication and division behave the same way.
# Boards have signal processing blocks on them that can process signals with basic functions: 
#    Add
#    Subtract
#    Multiply
#    Divide
# Signal blocks also can implement boolean logic on signals.  If a signal value is greater than 127 it is considered to be 'true' 
# (think of a voltage activating a transistor) else it is considered 'false'
# Boolean logic gates available include or, and, xor, nand, not.
# Signals are passed around through buffers, which can pull values from an input or push them to an output. The buffers are persistent in their values held.
# Signal blocks are arranced in a (3x3?) grid on a board.  The leftmost column of signal blocks can be connected to the board's inputs, and 
# the rightmost column can be connected to the outputs.  Blocks cannot connect backwards (to the left) or skip a column (from col 0 to col 2) 
# though there are 'thru' blocks which just pass signals and do no processing, so a block in col 0 could be connected to a 'thru' block in col 1
# and then to another block in col 2.
#
# Boards have a fixed number of inputs and outputs (3 of each) and can be connected to each other arbitrarily, so long as they are in the same backplane.
# Boards are connected in a 'backplane' which sets the order in which boards are evaluated (top to bottom)
# A backplane can hold a select number of boards (3?) and steps once through all board evaluations (eval 1, eval 2, eval 3) in a game tick.  
# In this way, if a board's output is connected to its own input we don't end up in an infinite regress.  

class Signal:
    def __init__(self,value):
        self.value = self.clamp(value)
    def add(self,sig):
        return Signal(self.clamp(self.value + sig.value))
    def sub(self,sig):
        return Signal(self.clamp(self.value - sig.value))
    def mul(self,sig):
        return Signal(self.clamp(self.value * sig.value))
    def div(self,sig):
        return Signal(self.clamp(float(self.value) / float(sig.value)))
    def avg(self,sig):
        return Signal(self.clamp((self.value + sig.value)/2))
    def clamp(self, val):
        if val > 255:
            return 255
        if val < 0:
            return 0
        return int(round(val))

    def toBin(self):
        if self.value > 127:
            return True
        else:
            return False
    def toVal(self,binval):
        if binval: 
            return Signal(255)
        else: 
            return Signal(0)

    def s_and(self,sig):
        return self.toVal(self.toBin() and sig.toBin())
    def s_or(self,sig):
        return self.toVal(self.toBin() or sig.toBin()) 
    def s_xor(self,sig):
        return self.toVal((self.toBin() ^ sig.toBin()))
    def s_nand(self,sig):
        return self.toVal(not(self.toBin() and sig.toBin()))
    def s_not(self):
        return Signal(not self.toBin())
    def __len__(self):
        return 1

class Buffer:
    iPort = None
    oPort = None
    sig = Signal(0)
    def __init__(self):
        pass
    def setInput(self,port):
        self.iPort = port
    def setOutput(self,port):
        self.oPort = port
    def set(self,sig):
        self.sig = sig
    def pull(self):
        if self.iPort is not None:
           self.set(self.iPort.get())
    def push(self):
        if self.oPort is not None:
            self.oPort.set(self.sig)
    def get(self):
        return self.sig

class Board:
    bi = []
    bo = []
    cols = 3
    rows = 3
    sigBlocks = [[None for i in range(rows)] for j in range(cols)]
    backplane = None 
    f = None
    def __init__(self):
        self.bi = [Buffer(),Buffer(),Buffer()]
        self.bo = [Buffer(),Buffer(),Buffer()]

    def connectToBoard(self,localBufIndex,brd,remoteBufIndex):
        if brd in self.backplane.boards:
            self.bo[localBufIndex].setOutput(brd.bi[remoteBufIndex])
            brd.bi[remoteBufIndex].setInput(self.bo[localBufIndex])
        else:
            pass
            # can't connect to boards they're not in your backplane

    def connectSensor(self, localBufIndex, sensor):
        sensor.bo.setOutput(self.bi[localBufIndex])
        self.bi[localBufIndex].setInput(sensor.bo)

    def connectActuator(self, localBufIndex, actuator):
        actuator.bi.setInput(self.bo[localBufIndex])
        self.bo[localBufIndex].setOutput(actuator.bi)

    def disconnect(self):
        for buf in self.bi:
            buf.iPort = None
        for buf in self.bo:
            buf.oPort = None
        self.backplane = None

    @staticmethod
    def checkindex(indices):
        map(checkindex,indices)
    @staticmethod
    def checkindex(ind):
        if ind > 2:
            pass # raise no such index error
        if ind < 0:
            pass

    def setSigBlock(self,sigblock,col,row):
        self.checkindex([col,row])
        self.sigBlocks[col][row] = sigblock

    def rmSigBlock(self,sigblock,col,row):
        sb = self.sigBlocks[col][row]
        if sb is not None:
            sb.disconnect()
        self.sigBlocks[col][row] = None

    def connectToSigBlock(self, biIndex, sigBlockRow, sigBlockInputIndex):
        # can only connect the first column of sigblocks to board inputs
        sigblock = self.sigBlocks[0][sigBlockRow]
        self.bi[biIndex].setOutput(sigblock.bi[sigBlockInputIndex])
        sigblock.bi[sigBlockInputIndex].setInput(self.bi[biIndex])

    def connectFromSigBlock(self, boIndex, sigBlockRow,sigBlockOutputIndex):
        # can only connect the last column of sigblocks to board outputs
        sigblock = self.sigBlocks[2][sigBlockRow]
        self.bo[boIndex].setInput(sigblock.bo[sigBlockOutputIndex])
        sigblock.bo[sigBlockOutputIndex].setOutput(self.bo[boIndex])

    def interconnectSB(self, outputSignalBlockCol, outputSignalBlockRow, outputSignalBlockIndex, 
        inputSignalBlockCol, inputSignalBlockRow, inputSignalBlockIndex):
        osb = self.sigBlocks[outputSignalBlockCol][outputSignalBlockRow]
        isb = self.sigBlocks[inputSignalBlockCol][inputSignalBlockRow]
        osbbuf = osb.bo[outputSignalBlockIndex]
        isbbuf = isb.bi[inputSignalBlockIndex]
        osbbuf.setOutput(isbbuf)
        isbbuf.setInput(osbbuf)

    def connectIpFromOp(self,iIndex,sb,oIndex):
        self.bi[iIndex].setInput(sb.bo[oIndex])
        sb.bo[oIndex].setOutput(self.bi[iIndex])

    def evaluate(self):
        map(Buffer.pull,self.bi)
        for col in self.sigBlocks:
            for block in col:
                if block is not None:
                    block.evaluate()
        map(Buffer.pull,self.bo)


class Backplane:
    boards = [None,None,None]
    def __init__(self):
        pass
    def setBoard(self,brd,slot):
        if self.boards[slot] == None:
            if brd.backplane == None:
                self.boards[slot] = brd
                brd.backplane = self

    def clrSlot(self,slot):
        if self.boards[slot] != None:
            boards[slot].disconnect()
            self.boards[slot] = None
        else:
            pass # already clear

    def rmBoard(self,brd):
        if brd in self.boards:
            brd.disconnect()
            boards[boards.indexOf(brd)] = None
        else:
            pass # no board to remove

    def tick(self):
        for board in self.boards:
            if board is not None:
                board.evaluate()

class SigBlock:
    bi = []
    bo = []
    f = None
    board = None
    def __init__(self, nInput, nOutput, function):
        self.bi = [Buffer() for i in range(nInput)]
        self.bo = [Buffer() for i in range(nOutput)]
        self.f = function
    def setBoard(self, board):
        self.board = board
    def evaluate(self):
        for b in self.bi:
            b.pull()
        result = self.f(*map(Buffer.get,self.bi))
        if len(result)>1:
            map(Buffer.set,self.bo,result)
        else:
            self.bo[0].set(result)
    def disconnect(self):
        pass


    # Could use introspection on signal to make these blocks maybe?
    @classmethod
    def add(cls): return cls(2,1,Signal.add)
    @classmethod
    def sub(cls): return cls(2,1,Signal.sub)
    @classmethod
    def mul(cls): return cls(2,1,Signal.mul)
    @classmethod
    def div(cls): return cls(2,1,Signal.div)
    @classmethod
    def avg(cls): return cls(2,1,Signal.avg)
    @classmethod
    def s_and(cls): return cls(2,1,Signal.s_and)
    @classmethod
    def s_or(cls): return cls(2,1,Signal.s_or)
    @classmethod
    def s_xor(cls): return cls(2,1,Signal.s_xor)
    @classmethod
    def s_nand(cls): return cls(2,1,Signal.s_nand)
    @classmethod
    def s_not(cls): return cls(1,1,Signal.s_not)
    @classmethod
    def thru1(cls): return cls(1,1,lambda x: x)
    @classmethod
    def thru2(cls): return cls(2,2,lambda x,y: (x,y))
    @classmethod
    def thru3(cls): return cls(3,3,lambda x,y,z: (x,y,z))
    @classmethod
    def fork(cls): return cls(1,2,lambda x: (x,x))
    @classmethod
    def pfrk(cls): return cls(1,3,lambda x: (x,x,x))


class ConstSensor:
    bo = None
    def __init__(self,value):
        self.bo = Buffer()
        self.bo.set(Signal(value))

class AlertActuator:
    # set up some monitoring on a buffer, maybe?
    bi = None
    def __init__(self):
        self.bi = Buffer()
    def alert(self):
        self.bi.pull()
        print(self.bi.get().value)
