
class Top():

  def __init__(self):
    pass

class Const(Top):

  def __init__():
    pass

class Atom(Const):

  def __init__(self, name):
    self.name = name

class Number(Const):

  def __init__(self, num):
    self.num = num

class Var(Top):

  def __init__(self, name):
    if not name[0].isupper(): raise "Must begin with uppercase Letters!"
    self.name = name

class Functor(Top):

  def __init__(self, name, *args):
    self.name = name
    self.args = args
    self.arity = len(args)

  def __eq__(self, obj):
    if isinstance(obj, Functor):
      return (self.name == obj.name and self.arity == obj.arity)
    return False

  def __ne__(self, obj):
    return not self == obj

def isVar(t):
  return isinstance(t, Var)

def isTop(t):
  return isinstance(t, Top)

def isConst(t):
  return isinstance(t, Const)

def isFunctor(t):
  return isinstance(t, Functor)

def unify(t1, t2):
  try:
    return unify_acc(t1, t2, [])
  except:
    return False

def unify_acc(t1, t2, collected):
  if isConst(t1) and isConst(t2) and t1.name == t2.name:
    return "%s = %s = %s" % (t1.name, t2.name, True)
  elif isVar(t1) and isTop(t2):
    return "%s = %s" % (t1.name, t2.name)
  elif isVar(t2) and isTop(t1):
    return "%s = %s" % (t2.name, t1.name)
  elif (isFunctor(t1) and isFunctor(t2) and t1 == t2):
    for (arg1, arg2) in zip(t1.args, t2.args):
      s = "%s = %s" % (t1.name, t2.name)
      collected.append((s, unify_acc(arg1, arg2, [])))
    return collected
  else:
		raise "Invalid!"

a1 = Atom("back")
a2 = Atom("abc")
v1 = Var("X")
v2 = Var("Y")
f = Functor(a1, a2)

t1 = Functor("f1", Functor("f2", v1, Functor("f3", v1)), Functor("f4", a1))
t2 = Functor("f1", Functor("f2", a2, Functor("f3", a1)), Functor("f4", v2))

print unify(t1, t2)




