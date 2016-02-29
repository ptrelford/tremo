module Sound

open System
open Microsoft.Xna.Framework.Audio

let sampleRate = 44100
let sample x = x * 32767. |> int16
let sampleLength duration = duration * float sampleRate |> int

let pi = Math.PI
let sineWave freq i = 
    sin (pi * 2. * float i / float sampleRate * freq)
let fadeOut duration i = 
    let sampleLength = sampleLength duration
    float (sampleLength - i) / float sampleLength
let tremolo freq depth i = (1.0 - depth) + depth * (sineWave freq i) ** 2.0

let toBytes (xs:int16[]) =
    let bytes = Array.CreateInstance(typeof<byte>, 2 * xs.Length)   
    Buffer.BlockCopy(xs, 0, bytes, 0, 2 * xs.Length)
    bytes :?> byte[]

let createSample f duration =
    let sampleLength = duration * float sampleRate |> int
    Array.init sampleLength (f >> min 1.0 >> max -1.0 >> sample)

let createEffect shape duration =
    let sample = createSample shape duration |> toBytes
    let blank = Array.create sampleRate 0uy // fix sound clicking
    let bytes = Array.append sample blank
    new SoundEffect(bytes, sampleRate, AudioChannels.Mono)       