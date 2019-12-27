import firebase from 'firebase/app';
import 'firebase/storage';
import {detectedError} from '../index';

let bgmAudio = new Audio();



/* ---------------------------------
	Subscriber
---------------------------------- */
export function audioSetUpSubscriber (app) {
  // 曲を取得する
  app.ports.setMusic.subscribe((audioFileName) => {
    getAudio(audioFileName);
  })

  // 曲を再生する
  app.ports.startMusic.subscribe(() => {
    if (bgmAudio.src) {
      bgmAudio.play();
      bgmAudio.currentTime = 0;
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

  // Home画面に戻っても音楽がなり続けるのを止める
  app.ports.startHomeMusic.subscribe(() => {
    bgmAudio.pause();
    bgmAudio.currentTime = 0
  })

  // Bgm音量を変更する
  app.ports.changeBgmVolume.subscribe((bgmVolume) => {
    bgmAudio.volume = bgmVolume;
  })
}



/* ---------------------------------
	曲を取得する
---------------------------------- */
const getAudio = (audioFileName) => {
  firebase.storage().ref(`audio/${audioFileName}.mp3`).getDownloadURL()
    .then(url => {
      bgmAudio.src = url;
    })
    .catch(detectedError)
}