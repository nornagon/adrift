# Computation model for adrift:
# Basic unit of compute is a "signal"
# signal is a value between 0 and 255.  It can represent an analog value (voltage, temperature, etc)
# in conidering it an analog value, adding two signals can saturate the channel.  The value does not 'roll over' like a digital value.
# similarly, subtracting can leave the channel empty - no 'reverse roll over' or negative values are possible.
# multiplication and division behave the same way.
# Boards can process signals with basic functions: 
#    Add
#    Subtract
#    Multiply
#    Divide
# Boards also can implement boolean logic on signals.  If a signal value is greater than 127 it is considered to be 'true' 
# (think of a voltage activating a transistor) else it is considered 'false'
# Boolean logic gates available include or, and, xor, nand, not.
# Boards have a fixed number of inputs and outputs (3 of each?) and can be connected to each other arbitrarily.
# Boards are connected in a 'backplane' which sets the order in which boards are evaluated (top to bottom) (and maybe also provides the external interface to sensors or actuators)
# A backplane can hold a select number of boards (5?) and steps once through all board evaluations (eval 1, eval 2, eval 3, eval 4, eval 5) in a game tick.  
# In this way, if a board's output is connected to its own input we don't end up in an infinite regress.  The IO buffers are persistent in their values held.



class Signal:
    def __init__(self,value):
        self.value = self.clamp(value)
    def add(self,sig):
        return self.clamp(self.value + sig.value)
    def sub(self,sig):
        return self.clamp(self.value - sig.value)
    def mul(self,sig):
        return self.clamp(self.value * sig.value)
    def div(self,sig):
        return self.clamp(float(self.value) / float(sig.value))
    def clamp(self, val):
        if val > 255:
            return Signal(255)
        if val < 0:
            return Signal(0)
        return Signal(int(round(val)))

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
    sig = None

    def __init__(self,ibrd,obrd):
        self.sig = Signal(0)
    def __init__(self,sig):
        self.sig = sig
    def setInput(self,port):
        self.iPort = port
    def setOutput(self,port):
        self.oPort = port
    def set(self,sig):
        self.sig = sig
    def get(self):
        return sig

class Board:
    i = [None,None,None]
    o = [None,None,None]
    backplane = None 
    f = None
    def __init__(self):
        self.i[1] = Buffer()
        self.i[1].seto(self)
        self.i[2] = Buffer()
        self.i[2].seto(self)
        self.i[3] = Buffer()
        self.i[3].seto(self)
        
        self.o[1] = Buffer()
        self.o[1].seti(self)
        self.o[2] = Buffer()
        self.o[2].seti(self)
        self.o[3] = Buffer()
        self.o[3].seti(self)

    def setFunction(self, f):    # HAHAHAHAHAHA It's totally definitely this easy
        self.f = f

    def evaluate(self):
        i1 = i[1].get
        i2 = i[1].get
        i3 = i[1].get
        out1,out2,out3 = self.f(i1,i2,i3)
        o[1].set(out1)
        o[2].set(out2)
        o[3].set(out3)

    def setBackplane(self,bp):
        self.backplane = bp

    def connectTo(self,local_buf_index,brd,remote_buf_index):
        if brd in self.backplane.boards:
            self.o[local_buf_index].setOutput(brd)
            brd.i[remote_buf_index].setInput(self.o[local_buf_index])
        else:
            pass
            # can't connect to boards they're not in your backplane
    def disconnect(self):
        for buf in i:
            buf.iPort = None
        for buf in o:
            buf.oPort = None
        self.backplane = None

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

class Sensor:
    pass

class Actuator:
    pass


class FunctionCreator:
    pass