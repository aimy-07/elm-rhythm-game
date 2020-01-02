import { Howl } from 'howler';



/* ---------------------------------
	Audio
---------------------------------- */
const createLoopBGM = (fileName, onload) =>
  new Howl({src: [`./audios/BGM/${fileName}.mp3`], loop: true , preload: false, onload});

const createNoLoopBGM = (fileName, onload, onend) =>
  new Howl({src: [`./audios/BGM/${fileName}.mp3`], loop: false, preload: false, onload, onend});

const createSE = (fileName, onload) =>
  new Howl({src: [`./audios/SE/${fileName}.mp3`] , loop: false, preload: false, onload});

export const BGM = (app) => {
  const loadedAudioInitial = () => {app.ports.loadedAudioInitial.send(null)};
  const loadedBGM = (fileName) => (() => {app.ports.loadedBGM.send(fileName)});
  const onEndBGM = () => {app.ports.onEndBGM.send(null)};

  return {
    theRoadToHeaven        : createLoopBGM  ('theRoadToHeaven', loadedAudioInitial),
    sampleSound            : createNoLoopBGM('sampleSound'            , loadedBGM('sampleSound')     , onEndBGM),
    sampleSoundShort       : createNoLoopBGM('sampleSoundShort'       , loadedBGM('sampleSoundShort'), onEndBGM),
    whiteGlow              : createNoLoopBGM('whiteGlow'              , loadedBGM('whiteGlow')       , onEndBGM),
    sampleSound_sample     : createLoopBGM  ('sampleSound_sample'     , loadedBGM('sampleSound_sample')),
    sampleSoundShort_sample: createLoopBGM  ('sampleSoundShort_sample', loadedBGM('sampleSoundShort_sample')),
    whiteGlow_sample       : createLoopBGM  ('whiteGlow_sample'       , loadedBGM('whiteGlow_sample')),
  }
}

export const SE = (app) => {
  const loadedSE = (fileName) => (() => {app.ports.loadedSE.send(fileName)});

  return {
    selectPlayMusic: createSE('selectPlayMusic', loadedSE('selectPlayMusic')),
  }
}



/* ---------------------------------
	Subscriber
---------------------------------- */
export function audioSetUpSubscriber (app, BGM, SE) {
  // タイトルのBGMだけ読み込む
  app.ports.loadAudioInitial.subscribe(() => {
    BGM.theRoadToHeaven.load();
  })

  // すべてのBGMを読み込む
  app.ports.loadBGM.subscribe(() => {
    for (let key in BGM) {
      if (key == 'theRoadToHeaven') continue;
      BGM[key].load();
    }
  })

  // すべてのSEを読み込む
  app.ports.loadSE.subscribe(() => {
    Object.keys(SE).forEach(key => {SE[key].load()});
  })

  // BGMを再生する
  app.ports.playBGM_.subscribe(({bgmKey, volume}) => {
    // 同じBGMがすでに再生されていたら再生しない
    if (BGM[bgmKey].seek() !== 0) return;
    // 再生中の音を止める
    for (let key in BGM) {
      if (BGM[key].playing()) BGM[key].stop();
    }
    BGM[bgmKey].volume(volume);
    BGM[bgmKey].play();
  })

  // SEを再生する
  app.ports.playSE_.subscribe(({seKey, volume}) => {
    SE[seKey].volume(volume);
    SE[seKey].play();
  })

  // BGMを一時停止する
  app.ports.pauseBGM_.subscribe(bgmKey => {
    BGM[bgmKey].pause();
  })

  // BGMを一時停止から再生する
  app.ports.unPauseBGM_.subscribe(bgmKey => {
    BGM[bgmKey].play();
  })

  // BGMを止める
  app.ports.stopBGM.subscribe(() => {
    for (let key in BGM) {
      BGM[key].stop();
    }
  })

  // BGMの現在の再生時間を取得する
  app.ports.getCurrentBGMTime_.subscribe(bgmKey => {
    const currentTime = BGM[bgmKey].seek();
    app.ports.gotCurrentBGMTime.send(currentTime);
  })

  // BGMの音量を変更する
  app.ports.changeBgmVolume.subscribe(volume => {
    for (let key in BGM) {
      BGM[key].volume(volume);
    }
  })
}