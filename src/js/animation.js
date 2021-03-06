/* ---------------------------------
	Subscriber
---------------------------------- */
export const animationSetUpSubscriber = (app) => {
  // 判定時の◇エフェクトアニメーションを再生する
  app.ports.playJudgeEffectAnim.subscribe(({key, isLongNote}) => {
    const judgeEffect = document.getElementById("judgeEffect_" + key);
    if (!judgeEffect) return;
    judgeEffect.classList.remove("long");
    if (isLongNote) {
      judgeEffect.classList.add("long");
    }
    replayAnim(judgeEffect);
  })

  // 判定文字のエフェクトアニメーションを再生する
  app.ports.playJudgeEffectTextAnim.subscribe(({key, judgeText}) => {
    const judgeEffectText = document.getElementById("judgeEffectText_" + key);
    if (!judgeEffectText) return;
    judgeEffectText.textContent = judgeText;
    replayAnim(judgeEffectText);
  })

  // ミスエフェクトアニメーションを再生する
  app.ports.playMissEffectAnim.subscribe(() => {
    const missEffect = document.getElementById("missEffect");
    replayAnim(missEffect);
  })

  // コンボアニメーションを再生する
  app.ports.playComboEffectAnim.subscribe(() => {
    const comboText = document.getElementById("comboText");
    replayAnim(comboText);
  })

  // 曲選択のアニメーションを再生する
  app.ports.playMusicSelectAnim.subscribe(() => {
    const centerArea = document.getElementById("home_centerArea");
    replayAnim(centerArea);
    const topLeftArea = document.getElementById("home_topLeftArea");
    replayAnim(topLeftArea);
    const topRightArea = document.getElementById("home_topRightArea");
    replayAnim(topRightArea);
    const bottomLeftArea1 = document.getElementById("home_bottomLeftArea1");
    replayAnim(bottomLeftArea1);
    const bottomLeftArea2 = document.getElementById("home_bottomLeftArea2");
    replayAnim(bottomLeftArea2);
    const bottomRightArea = document.getElementById("home_bottomRightArea");
    replayAnim(bottomRightArea);
    if (!bottomRightArea) return;
    // home_bottomRightArea のアニメーションを通常アニメーションに戻す
    setTimeout(() => bottomRightArea.classList.remove("playAnim"), 400);
  })
}



/* ---------------------------------
	アニメーションを再生する
---------------------------------- */
const replayAnim = (element) => {
  if (!element) return;
  element.classList.remove("playAnim");
  requestAnimationFrame(() => {
    element.classList.add("playAnim");
  });
}