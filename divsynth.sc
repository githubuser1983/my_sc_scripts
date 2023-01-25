(
~divisors = {
	arg n;
	~divs = List.newClear(0);
	for( 1, floor(sqrt(n)), {arg i; if(n%i==0,{
			i.postln;
			~divs.add(i);
			(i==n.div(i)).postln;
			if(i==n.div(i),{},{~divs.add(n.div(i))});
		   },{});
		});
	~divs.sort;
	~divs;
};

SynthDef(\divsynth, { |out=0,divs = #[1],amp=1,sustain=1|
    var l,sig,env,n,sortedDivs;
    l = divs;
	n = divs.at(divs.size-1);
	sig = SinOsc.ar(1, 0, 0);
	l.do({ arg item, i;
        d = item;
		sig = sig+SinOsc.ar(d, 0, amp );
	});
	sig = sig*EnvGen.kr(Env.linen(0.001, sustain, 0.1), doneAction: Done.freeSelf);
    Out.ar(out, sig ! 2)
}).add;

r =Ppar([
	     Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/log(x),x<-(1000..1002)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1002)},3*5),
			\sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1002)},3*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/log(x),x<-(1000..1003)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1003)},2*5),
	    \sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1003)},2*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/log(x),x<-(1000..1005)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1005)},2*3),
			\sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1005)},2*3)
	)],1);

s =Ppar([
	     Pbind(\instrument,\divsynth,
	      \amp,0.065,
			\dur,Pseq(all{:1/(2*log(x)),x<-(500..502)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(500..502)},3*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/(2*log(x)),x<-(500..503)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(500..503)},2*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/(2*log(x)),x<-(500..505)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(500..505)},2*3)
	)],1);


t =Ppar([
	     Pbind(\instrument,\divsynth,
	      \amp,0.065,
			\dur,Pseq(all{:1/(3*log(x)),x<-(1000..1002)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1002)},3*5),
		\sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1002)},3*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/(3*log(x)),x<-(1000..1003)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1003)},2*5),
		 \sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1003)},2*5)
         ),
	   Pbind(\instrument,\divsynth,
	      \amp,0.065,
	      \dur,Pseq(all{:1/(3*log(x)),x<-(1000..1005)},inf),
	     \divs,Pseq(all{:~divisors.value(x),x<-(1000..1005)},2*3),
		\sustain,Pseq(all{:log(x)/sqrt(x),x<-(1000..1005)},2*3)
	)],1);

m = Pseq([Pseq([s],1),
	Pseq([t],1),
	Ppar([t,s],2),
	Pseq([r],3),
	  Ppar([r,s],1),
	  Ppar([t,r],2),
	Ppar([r,s,t],3),
	Ppar([t,r],2),
	Ppar([r,s],1),
	Pseq([r],1)
],inf);

Pdef(\mplayer, m).clock_(TempoClock(40/60)).play;
// fade out over 8 beats
//x= m.play(TempoClock(40/60));

);

Pdef(\mplayer, Pn(Event.silent)).fadeTime_(8);
Pdef(\mplayer).stop;
