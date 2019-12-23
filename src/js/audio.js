import firebase from 'firebase/app';
import 'firebase/storage';
import {detectedError} from '../index';

let musicAudio = new Audio();



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
    if (musicAudio.src) {
      musicAudio.play();
      musicAudio.currentTime = 0;
      app.ports.gotCurrentMusicTime.send(0);
    } else {
      detectedError('musicAudio is undefined');
    }
  })

  // 曲を一時停止する
  app.ports.pauseMusic.subscribe(() => {
    musicAudio.pause();
  })

  // 曲を一時停止から再生する
  app.ports.unPauseMusic.subscribe(() => {
    musicAudio.play();
    const currentMusicTime = musicAudio.currentTime * 1000;
    app.ports.gotCurrentMusicTime.send(currentMusicTime);
  })

  // 曲の現在の再生時間を取得する
  app.ports.getCurrentMusicTime.subscribe(() => {
    const currentMusicTime = musicAudio.currentTime * 1000;
    app.ports.gotCurrentMusicTime.send(currentMusicTime);
  })

  // タップ音を鳴らす
  app.ports.playTapSound.subscribe(() => {
    // TODO: 重くならないように工夫する
    // 音発火が重い
    // const tapAudio = new Audio();
    // tapAudio.src = "./audios/tapSound2.wav";
    // tapAudio.play();
  })

  app.ports.startHomeMusic.subscribe(() => {
    musicAudio.pause();
    musicAudio.currentTime = 0
  })
}



/* ---------------------------------
	曲を取得する
---------------------------------- */
const getAudio = (audioFileName) => {
  firebase.storage().ref(`audio/${audioFileName}.mp3`).getDownloadURL()
    .then(url => {
      musicAudio.src = url;
    })
    .catch(detectedError)
}