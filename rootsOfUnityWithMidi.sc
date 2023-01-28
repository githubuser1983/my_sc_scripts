// How does it sound? https://www.youtube.com/watch?v=5VeNFqaQe2o
(

SynthDef(\kick, {
    |out = 0, pan = 0, amp = 0.3|
    var body, bodyFreq, bodyAmp;
    var pop, popFreq, popAmp;
    var click, clickAmp;
    var snd;

    // body starts midrange, quickly drops down to low freqs, and trails off
    bodyFreq = EnvGen.ar(Env([261, 120, 51], [0.035, 0.08], curve: \exp));
    bodyAmp = EnvGen.ar(Env.linen(0.005, 0.1, 0.3), doneAction: 2);
    body = SinOsc.ar(bodyFreq) * bodyAmp;
    // pop sweeps over the midrange
    popFreq = XLine.kr(750, 261, 0.02);
    popAmp = EnvGen.ar(Env.linen(0.001, 0.02, 0.001)) * 0.15;
    pop = SinOsc.ar(popFreq) * popAmp;
    // click is spectrally rich, covering the high-freq range
    // you can use Formant, FM, noise, whatever
    clickAmp = EnvGen.ar(Env.perc(0.001, 0.01)) * 0.15;
    click = LPF.ar(Formant.ar(910, 4760, 2110), 3140) * clickAmp;

    snd = body + pop + click;
    snd = snd.tanh;

    Out.ar(out, Pan2.ar(snd, pan, amp));
}).add;


SynthDef(\snare, {
    |out = 0, pan = 0, amp = 0.3|
    var pop, popAmp, popFreq;
    var noise, noiseAmp;
    var snd;

    // pop makes a click coming from very high frequencies
    // slowing down a little and stopping in mid-to-low
    popFreq = EnvGen.ar(Env([3261, 410, 160], [0.005, 0.01], curve: \exp));
    popAmp = EnvGen.ar(Env.perc(0.001, 0.11)) * 0.7;
    pop = SinOsc.ar(popFreq) * popAmp;
    // bandpass-filtered white noise
    noiseAmp = EnvGen.ar(Env.perc(0.001, 0.15), doneAction: 2);
    noise = BPF.ar(WhiteNoise.ar, 810, 1.6) * noiseAmp;

    snd = (pop + noise) * 1.3;

    Out.ar(out, Pan2.ar(snd, pan, amp));
}).add;

SynthDef(\hihat, {
    |out = 0, pan = 0, amp = 0.3|
    var click, clickAmp;
    var noise, noiseAmp;
    var snd;

    // noise -> resonance -> expodec envelope
    noiseAmp = EnvGen.ar(Env.perc(0.001, 0.3, curve: -8), doneAction: 2);
    noise = Mix(BPF.ar(ClipNoise.ar, [4010, 4151], [0.15, 0.56], [1.0, 0.6])) * 0.7 * noiseAmp;

    snd = noise;

    Out.ar(out, Pan2.ar(snd, pan, amp));
}).add;

// adapted from a post by Neil Cosgrove (other three are original)
SynthDef(\clap, {
    |out = 0, amp = 0.5, pan = 0, dur = 1|
    var env1, env2, snd, noise1, noise2;

    // noise 1 - 4 short repeats
    env1 = EnvGen.ar(
        Env.new(
            [0, 1, 0, 0.9, 0, 0.7, 0, 0.5, 0],
            [0.001, 0.009, 0, 0.008, 0, 0.01, 0, 0.03],
            [0, -3, 0, -3, 0, -3, 0, -4]
        )
    );

    noise1 = WhiteNoise.ar(env1);
    noise1 = HPF.ar(noise1, 600);
    noise1 = LPF.ar(noise1, XLine.kr(7200, 4000, 0.03));
    noise1 = BPF.ar(noise1, 1620, 3);

    // noise 2 - 1 longer single
    env2 = EnvGen.ar(Env.new([0, 1, 0], [0.02, 0.18], [0, -4]), doneAction:2);

    noise2 = WhiteNoise.ar(env2);
    noise2 = HPF.ar(noise2, 1000);
    noise2 = LPF.ar(noise2, 7600);
    noise2 = BPF.ar(noise2, 1230, 0.7, 0.7);

    snd = noise1 + noise2;
    snd = snd * 2;
    snd = snd.softclip;

    Out.ar(out, Pan2.ar(snd,pan,amp));
}).add;


~divisors = {
	arg n;
	~divs = List.newClear(0);
	for( 1, floor(sqrt(n)), {arg i; if(n%i==0,{
			//i.postln;
			~divs.add(i);
		//	(i==n.div(i)).postln;
			if(i==n.div(i),{},{~divs.add(n.div(i))});
		   },{});
		});
	~divs.sort;
	~divs;
};



SynthDef(\rootsOfUnitySum, { |out=0,freq=440,amp=1,n=1,ks=#[1]|
    var l,sigl,sigr,env;


	sigl = SinOsc.ar(freq:0,mul:0);
	sigr = SinOsc.ar(freq:0,mul:0);
	ks.do({arg item,i;
		k = item;
        sigr = sigr+SinOsc.ar(freq:freq*2pi*k/n,mul:amp,phase:0.5*pi);
		sigl = sigl+SinOsc.ar(freq:freq*2pi*k/n,mul:amp);
	});
	env = EnvGen.kr(Env.linen(attackTime: 0.001, sustainTime: 1, releaseTime: 0.01, level: 1.0, curve: 'lin'), doneAction: Done.freeSelf);
	sigr = sigr*env;
    sigl = sigl*env;

	Out.ar(out, [sigl,sigr])
}).add;



~getPbind = { arg n, amps, durs, freq;
    ~nums = ~divisors.value(n);
    r = Pbind(\instrument,\rootsOfUnitySum,
	\amp,amps,
	\dur,durs,
	\freq,freq,
	\n,Pseq([n],~nums.size),
	\ks , Pseq(all{:~divisors.value(n),n<-~nums},1)).trace();
	r
};


n = SimpleMIDIFile.read( "~/notebooks/musik/midi/InfinitePiChallenge3-Violin-Cello.mid" );

~m1 = all{:ev.at(4),ev <- n.midiTrackEvents(1),ev.at(2)==\noteOn};
~tau1 = all{:~divisors.value(n).size,n<-~m1};

~m2 = all{:ev.at(4),ev <- n.midiTrackEvents(2),ev.at(2)==\noteOn};
~tau2 = all{:~divisors.value(n).size,n<-~m2};

~m3 = all{:ev.at(4),ev <- n.midiTrackEvents(3),ev.at(2)==\noteOn};
~tau3 = all{:~divisors.value(n).size,n<-~m3};

~m4 = all{:ev.at(4),ev <- n.midiTrackEvents(4),ev.at(2)==\noteOn};
~tau4 = all{:~divisors.value(n).size,n<-~m4};


~r = Pseq(all{:~getPbind.value(
	  n:n,
	  amps:Pseq(all{:0.5/t,t<-~tau1},1),
	  durs:0.5,
	freq:n.midicps/4),n<-~m1},1);

~s = Pseq(all{:~getPbind.value(
	  n:n,
	  amps:Pseq(all{:0.5/t,t<-~tau2},1),
	  durs:0.5,
	freq:n.midicps/4),n<-~m2},1);

~t = Pseq(all{:~getPbind.value(
	  n:n,
	  amps:Pseq(all{:0.5/t,t<-~tau3},1),
	  durs:0.5,
	freq:n.midicps/4),n<-~m3},1);

~u = Pseq(all{:~getPbind.value(
	  n:n,
	  amps:Pseq(all{:0.5/t,t<-~tau4},1),
	  durs:0.5,
	freq:n.midicps/4),n<-~m4},1);


~p=Ppar([
	Pbindf(
		Pbind(\amp,0.125),
        \instrument, Pseq([\kick, \snare, \kick, \kick, \snare], inf),
        \dur, Pseq([4/4, 3/4, 3/4, 2/4, 4/4], 10*~m1.size)
    ),
    Pbindf(
        Pbind(\amp,0.125),
        \instrument, Pseq([Pn(\hihat, 16), Pn(\clap, 16)], inf),
        \dur, Pseq([Rest(2/4), 2/4, Rest(2/4), 2/4], 10*~m1.size)
    )
]);

~y = Ppar([~r,~s,~t,~u,~p],1);
);

(
Server.default.record;
Pdef(\mplayer, ~y).clock_(TempoClock(100/60)).play;
);


Pdef(\mplayer, Pn(Event.silent)).fadeTime_(8);
Server.default.stopRecording;

