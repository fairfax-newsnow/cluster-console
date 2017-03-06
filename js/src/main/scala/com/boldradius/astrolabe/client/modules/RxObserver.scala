package com.boldradius.astrolabe.client.modules

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.OnUnmount
import rx._
import rx.ops._ //implicit foreach on rx

abstract class RxObserver[BS <: BackendScope[_, _]](scope: BS) extends OnUnmount {
  protected def observe[T](rx: Rx[T]): Unit = {
    val obs = rx.foreach(_ => scope.forceUpdate.runNow(), skipInitial = true)
    // stop observing when unmounted
    onUnmount(Callback(obs.kill()))
  }

  /**
   * Instead of forceUpdate(), call an update function
   */
  protected def react[T](rx: Rx[T], update: T => Unit): Unit = {
    val obs = rx.foreach(v => {
      update(v)
    }, skipInitial = true)

    onUnmount(Callback(obs.kill()))
  }

}
