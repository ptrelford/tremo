﻿module Utils

open System
open System.Windows.Controls

let synchronize f = 
  let ctx = System.Threading.SynchronizationContext.Current 
  f (fun g arg ->
    let nctx = System.Threading.SynchronizationContext.Current 
    if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g(arg)), null)
    else g(arg) )

type Microsoft.FSharp.Control.Async with 
  static member AwaitObservable(ev1:IObservable<'a>) =
    synchronize (fun f ->
      Async.FromContinuations((fun (cont,econt,ccont) -> 
        let rec callback = (fun value ->
          remover.Dispose()
          f cont value )
        and remover : IDisposable  = ev1.Subscribe(callback) 
        () )))
  
  static member AwaitObservable(ev1:IObservable<'a>, ev2:IObservable<'b>) = 
    synchronize (fun f ->
      Async.FromContinuations((fun (cont,econt,ccont) -> 
        let rec callback1 = (fun value ->
          remover1.Dispose()
          remover2.Dispose()
          f cont (Choice1Of2(value)) )
        and callback2 = (fun value ->
          remover1.Dispose()
          remover2.Dispose()
          f cont (Choice2Of2(value)) )
        and remover1 : IDisposable  = ev1.Subscribe(callback1) 
        and remover2 : IDisposable  = ev2.Subscribe(callback2) 
        () )))
