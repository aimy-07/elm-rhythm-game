import { Howl } from 'howler';



/* ---------------------------------
	Audio
---------------------------------- */
const createBGM = (fileName, loop, onload) => new Howl({src: [`./audios/BGM/${fileName}.mp3`], loop, preload: false, onload});
const createSE = (fileName, onload) => new Howl({src: [`./audios/SE/${fileName}.mp3`], loop: false, preload: false, onload});

export const BGM = (app) => {
  return {
    theRoadToHeaven: createBGM('theRoadToHeaven', true, () => {app.ports.loadedAudioInitial.send(null)}),
    sampleSound: createBGM('sampleSound', false, () => {app.ports.loadedBGM.send('sampleSound')}),
    sampleSoundShort: createBGM('sampleSoundShort', false, () => {app.ports.loadedBGM.send('sampleSoundShort')}),
    whiteGlow: createBGM('whiteGlow', false, () => {app.ports.loadedBGM.send('whiteGlow')}),
    sampleSound_sample: createBGM('sampleSound_sample', true, () => {app.ports.loadedBGM.send('sampleSound_sample')}),
    sampleSoundShort_sample: createBGM('sampleSoundShort_sample', true, () => {app.ports.loadedBGM.send('sampleSoundShort_sample')}),
    whiteGlow_sample: createBGM('whiteGlow_sample', true, () => {app.ports.loadedBGM.send('whiteGlow_sample')}),
  }
}

export const SE = (app) => {
  return {
    selectPlayMusic: createSE('selectPlayMusic', () => {app.ports.loadedSE.send('selectPlayMusic')}),
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

  // BGMの音量を変更する（Home）
  app.ports.changeBgmVolume.subscribe(volume => {
    for (let key in BGM) {
      BGM[key].volume(volume);
    }
  })
}