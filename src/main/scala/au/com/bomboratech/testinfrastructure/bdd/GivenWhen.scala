package au.com.bomboratech.testinfrastructure.bdd

trait GivenWhen {

  protected def given[T](context: Context[T]): T = context()

  protected def when[T](action: Action[T]): T = action()
}

trait Context[+T] extends (() => T) {
  def apply(): T

  def and[U](that: Context[U]): Context[U] = {
    val self = this
    Context[U](() => {
      self()
      that()
    })
  }

  //This looks like flatMap. Is Context a MONAD?!!
  def andThen[U](that: (T => Context[U])): Context[U] = {
    val self = this
    Context[U](() => that(self())())
  }

  def map[U](that: (T => U)): Context[U] = {
    val self = this
    Context[U](() => that(self()))
  }

  def whilst[U](around: Around[U]): Context[U] = Context(around.before) and this and Context(around.after)
}

object Context {

  def apply[T](context: () => T) = new Context[T] {
    def apply = context()
  }
}

class ContextMap[T, U](map: T => Context[U]) extends (T => Context[U]) {

  def apply(t: T) = map(t)

  def and[V](other: T => Context[V]): ContextMap[T, V] = {
    val self = this
    ContextMap[T, V](t => self(t) and other(t))
  }
}

object ContextMap {

  def apply[T, U](map: T => Context[U]): ContextMap[T, U] = new ContextMap[T, U](map)

  implicit def functionToContextMap[T, U](map: T => Context[U]): ContextMap[T, U] = ContextMap(map)
}

trait Action[+T] extends (() => T) {
  def apply(): T

  // Actions are not supposed to be composed in tests.
  // Test cases should be explicits about all the actions triggered in the "when" part
  def and[U](that: Action[U]): Action[U] = {
    val self = this
    Action[U](() => {
      self()
      that()
    })
  }

  def whilst[U](around: Around[U]): Action[U] = Action(around.before) and this and Action(around.after)

}

object Action {

  def apply[T](action: () => T) = new Action[T] {
    def apply = action()
  }

}

case class Around[T](before: () => Unit, after: () => T)
