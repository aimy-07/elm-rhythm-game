import firebase from 'firebase/app';
import 'firebase/storage';
import {detectedError} from '../index';

let bgmAudio = new Audio();
let sampleAudioSrcList = {}



/* ---------------------------------
	Subscriber
---------------------------------- */
export function audioSetUpSubscriber (app) {
  // 曲を取得する
  app.ports.setMusic.subscribe((audioFileName) => {
    bgmAudio.pause();
    bgmAudio.currentTime = 0;
    bgmAudio.loop = false;
    getAudio(audioFileName)
      .then((url) => {
        bgmAudio.src = url
      })
      .catch(detectedError)
  })

  // 曲を再生する
  app.ports.startMusic.subscribe(() => {
    if (bgmAudio.src) {
      bgmAudio.play();
      app.ports.gotCurrentMusicTime.send(0);
    } else {
      detectedError('bgmAudio is undefined');
    }
  })

  // 曲を一時停止する
  app.ports.pauseMusic.subscribe(() => {
    bgmAudio.pause();
  })

  // 曲を一時停止から再生する
  app.ports.unPauseMusic.subscribe(() => {
    bgmAudio.play();
    const currentMusicTime = bgmAudio.currentTime * 1000;
    app.ports.gotCurrentMusicTime.send(currentMusicTime);
  })

  // 曲の現在の再生時間を取得する
  app.ports.getCurrentMusicTime.subscribe(() => {
    const currentMusicTime = bgmAudio.currentTime * 1000;
    app.ports.gotCurrentMusicTime.send(currentMusicTime);
  })

  // HomeBgmを取得する
  app.ports.getAllSampleAudio.subscribe((audioFileNames) => {
    const getSampleAudioSrcList = audioFileNames.map(audioFileName => 
      getSampleAudio(audioFileName)
        .then((url) => {
          return {[audioFileName]: url}
        })
        .catch(detectedError)
    );
    Promise.all(getSampleAudioSrcList)
      .then((sampleAudioSrcs) => {
        sampleAudioSrcs.forEach(sampleAudioSrc => 
          Object.assign(sampleAudioSrcList, sampleAudioSrc)
        )
        console.log(sampleAudioSrcList);
        app.ports.gotAllSampleAudio.send(null);
      })
      .catch(detectedError)
  })

  // HomeBgmを再生・変更する
  app.ports.playHomeBgm.subscribe((audioFileName) => {
    bgmAudio.pause();
    bgmAudio.currentTime = 0
    bgmAudio.src = sampleAudioSrcList[audioFileName];
    bgmAudio.loop = true;
    bgmAudio.play();
  })

  // Bgm音量を変更する
  app.ports.changeBgmVolume.subscribe((bgmVolume) => {
    bgmAudio.volume = bgmVolume;
  })

  // TitleBgmを再生する
  app.ports.playTitleBgm.subscribe(() => {
    bgmAudio.pause();
    bgmAudio.currentTime = 0
  })
}



/* ---------------------------------
	曲を取得する
---------------------------------- */
const getAudio = (audioFileName) => {
  return firebase.storage().ref(`audio/${audioFileName}.mp3`).getDownloadURL()
    .then(url => url)
    .catch(detectedError)
}

const getSampleAudio = (audioFileName) => {
  return firebase.storage().ref(`audio/${audioFileName}_sample.mp3`).getDownloadURL()
    .then(url => url)
    .catch(detectedError)
}