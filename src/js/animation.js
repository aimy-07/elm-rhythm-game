/* ---------------------------------
	Subscriber
---------------------------------- */
export function animationSetUpSubscriber (app) {
  // 判定時の◇エフェクトアニメーションを再生する
  app.ports.playJudgeEffectAnim.subscribe(({keyStr, noteType}) => {
    const judgeEffect = document.getElementById("judgeEffect_" + keyStr);
    judgeEffect.classList.remove("long");
    if (noteType === "LONG") {
      judgeEffect.classList.add("long");
    }
    judgeEffect.classList.remove("playAnim");
    requestAnimationFrame(() => {
      judgeEffect.classList.add("playAnim");
    });
  })

  // 判定文字のエフェクトアニメーションを再生する
  app.ports.playJudgeEffectTextAnim.subscribe(({keyStr, judgeText}) => {
    const judgeEffectText = document.getElementById("judgeEffectText_" + keyStr);
    judgeEffectText.textContent = judgeText;
    judgeEffectText.classList.remove("playAnim");
    requestAnimationFrame(() => {
      judgeEffectText.classList.add("playAnim");
    });
  })

  // コンボアニメーションを再生する
  app.ports.playComboEffectAnim.subscribe(() => {
    const comboText = document.getElementById("comboText");
    comboText.classList.remove("playAnim");
    requestAnimationFrame(() => {
      comboText.classList.add("playAnim");
    });
  })
}