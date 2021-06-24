type t = float;
let gettimeofday = (): t => Js.Date.now();
let sleep = ms => {
  Js.Promise.(
    make((~resolve, ~reject as _reject) => {
      let value = ();
      Js.Global.setTimeout(() => resolve(. value), ms)->ignore;
    })
  );
};

let localtime = (~time: float) => {
  let seconds = time->Js.Date.fromFloat->Js.Date.getSeconds;
  let seconds = seconds->int_of_float;
  let minutes = time->Js.Date.fromFloat->Js.Date.getMinutes;
  let minutes = minutes->int_of_float;
  let hours = time->Js.Date.fromFloat->Js.Date.getHours;
  let hours = hours->int_of_float;
  (seconds, minutes, hours);
};
