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
           self.set(iPort.get)
    def push(self):
        if self.oPort is not None:
            self.oPort.set(sig)
    def get(self):
        return self.sig

class Board:
    bi = [Buffer(),Buffer(),Buffer()]
    bo = [Buffer(),Buffer(),Buffer()]
    cols = 3
    rows = 3
    sigBlocks = [[None for i in range(rows)] for j in range(cols)]
    backplane = None 
    f = None
    def __init__(self):
        pass
    def evaluate(self):
        map(Buffer.pull,self.bi)
        for col in sigBlocks:
            for block in col:
                block.pull()
                block.execute()
        map(Buffer.pull,self.bo)

    def setBackplane(self,bp):
        self.backplane = bp

    def connectTo(self,local_buf_index,brd,remote_buf_index):
        if brd in self.backplane.boards:
            self.bo[local_buf_index].setOutput(brd.bi[remote_buf_index])
            brd.bi[remote_buf_index].setInput(self.bo[local_buf_index])
        else:
            pass
            # can't connect to boards they're not in your backplane

    def disconnect(self):
        for buf in self.bi:
            buf.iPort = None
        for buf in self.bo:
            buf.oPort = None
        self.backplane = None
    def checkindex(indices):
        map(checkindex,indices)
    def checkindex(ind):
        if ind > 2:
            pass # raise no such index error
        if ind < 0:
            pass
    def setSigBlock(self,sigblock,col,row):
        self.checkindex([col,row])
        self.sigBlocks[col][row] = sigblock

    def rmSigBlock(self,sigblock,row,col):
        sb = self.sigBlocks[col][row]
        if sb is not None:
            sb.disconnect()
        self.sigBlocks[col][row] = None

    def connectToSigBlock(self, biIndex, sigBlockRow, sigBlockInputIndex):
        # can only connect the first column of sigblocks to board inputs
        sigblock = self.sigBlocks[0][sigBlockRow]
        self.bi[biIndex].setOutput(sigblock.bi[sigBlockInputIndex])
        sigblock.bi[sigBlockInputIndex].setInput(self.bi[biIndex])
        pass

    def connectFromSigBlock(self, boIndex, sigBlockRow,sigBlockOutputIndex):
        # can only connect the last column of sigblocks to board outputs
        sigblock = self.sigBlocks[2][sigBlockRow]
        self.bo[boIndex].setInput(sigblock.bo[sigBlockOutputIndex])
        sigblock.bo[sigBlockOutputIndex].setOutput(self.bo[boIndex])
        pass


class Backplane:
    boards = [None,None,None]
    def __init__(self):
        pass
    def setBoard(self,brd,slot):
        if boards[slot] == None:
            if brd.backplane == None:
                self.boards[slot] = brd
                brd.backplane = self

    def clrSlot(self,slot):
        if boards[slot] != None:
            boards[slot].disconnect()
            self.boards[slot] = None
        else:
            pass # already clear

    def rmBoard(self,brd):
        if brd in boards:
            brd.disconnect()
            boards[boards.indexOf(brd)] = None
        else:
            pass # no board to remove

    def tick(self):
        for board in boards:
            board.evaluate()

class SigBlock:
    bi = []
    bo = []
    f = None
    def __init__(self, nInput, nOutput, function):
        self.bi = [Buffer() for i in range(nInput)]
        self.bo = [Buffer() for i in range(nOutput)]
        self.f = function
    def execute(self):
        result = f(*map(Buffer.get(),bi))
        map(Buffer.set,bo,result)
    def disconnect(self):
        pass
    def connectOpToIp(self,oIndex,sb,iIndex):
        self.bo[oIndex].setOutput(sb.bi[iIndex])
        sb.bi[iIndex].setInput(self.bo[oIndex])
    def connectIpFromOp(self,iIndex,sb,oIndex):
        self.bi[iIndex].setInput(sb.bo[oIndex])
        sb.bo[oIndex].setOutput(self.bi[iIndex])

# Could use introspection to make these blocks maybe?
class SigBlocks:
    add = SigBlock(2,1,Signal.add)
    sub = SigBlock(2,1,Signal.sub)
    mul = SigBlock(2,1,Signal.mul)
    div = SigBlock(2,1,Signal.div)
    avg = SigBlock(2,1,Signal.avg)
    s_and = SigBlock(2,1,Signal.s_and)
    s_or = SigBlock(2,1,Signal.s_or)
    s_xor = SigBlock(2,1,Signal.s_xor)
    s_nand = SigBlock(2,1,Signal.s_nand)
    s_not = SigBlock(1,1,Signal.s_not)
    thru1 = SigBlock(1,1,lambda x: x)
    thru2 = SigBlock(2,2,lambda x,y: x,y)
    thru3 = SigBlock(3,3,lambda x,y,z: x,y,z)

class Sensor:
    pass

class Actuator:
    pass
