
class Pitfall:
    def __init__(self, id, loc, op):
        self.id=id
        self.loc=loc
        self.op = op

    def __hash__(self):
        return hash(self.op + self.id)
    
    def __eq__(self, value):
        return self.__hash__() == value.__hash__()