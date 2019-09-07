let musicAudio;

/* ---------------------------------
	Subscriber
---------------------------------- */
export function audioSetUpSubscriber (app) {
  // 曲を再生する
  app.ports.startMusic.subscribe(() => {
    musicAudio = new Audio();
    musicAudio.src = "./audios/sample_sound.wav";
    musicAudio.play();
    app.ports.gotCurrentMusicTime.send(0); 
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
}