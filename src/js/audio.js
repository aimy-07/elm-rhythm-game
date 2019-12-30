import firebase from 'firebase/app';
import 'firebase/storage';
import {detectedError} from '../index';

const bgmAudio = new Audio();



/* ---------------------------------
	Subscriber
---------------------------------- */
export function audioSetUpSubscriber (app) {
  // BGMのUrlを取得する（共通）
  app.ports.getAudioInfo.subscribe(audioFileName => {
    bgmAudio.pause();
    firebase.storage().ref(`audio/${audioFileName}.mp3`).getDownloadURL()
      .then((audioUrl) => {
        console.log({audioFileName, audioUrl});
        app.ports.gotAudioInfo.send({audioFileName, audioUrl});
      })
      .catch(detectedError)
  })

  // BGMを再生する（共通）
  app.ports.playBGM_.subscribe(({audioUrl, volume, isLoop}) => {
    bgmAudio.pause();
    bgmAudio.src = audioUrl;
    bgmAudio.volume = volume;
    bgmAudio.loop = isLoop;
    bgmAudio.currentMusicTime = 0;
    bgmAudio.play();
  })

  // BGMを一時停止する（共通）
  app.ports.pauseBGM.subscribe(() => {
    bgmAudio.pause();
  })

  // BGMを一時停止から再生する（共通）
  app.ports.unPauseBGM.subscribe(() => {
    bgmAudio.play();
  })

  // 曲の現在の再生時間を取得する（Play）
  app.ports.getCurrentMusicTime.subscribe(() => {
    const currentMusicTime = bgmAudio.currentTime * 1000;
    app.ports.gotCurrentMusicTime.send(currentMusicTime);
  })

  // BGMの音量を変更する（Home）
  app.ports.changeBgmVolume.subscribe(volume => {
    bgmAudio.volume = volume;
  })

  // FIXME: TitleページのBgmを再生する(仮)（Title）
  app.ports.playTitleBgm.subscribe(() => {
    bgmAudio.pause();
  })

  // SEを再生する
  app.ports.playSE_.subscribe(({audioUrl, volume}) => {
    const seAudio = new Audio();
    seAudio.src = audioUrl;
    seAudio.volume = volume;
    seAudio.play();
  })
}