/* ---------------------------------
	Subscriber
---------------------------------- */
export const trackingSetUpSubscriber = (app, firebaseAnalytics) => {
  app.ports.trackingPlayStart.subscribe(csvFileName => {
    firebaseAnalytics.logEvent(trackingEvent.playStart, {csvFileName});
  })

  app.ports.trackingPlayEnd.subscribe(csvFileName => {
    firebaseAnalytics.logEvent(trackingEvent.playEnd, {csvFileName});
  })
}



/* ---------------------------------
	Event
---------------------------------- */
export const trackingEvent = {
  playStart: 'PLAY_START',
  playEnd: 'PLAY_END',
  visit: 'VISIT',
  visitAndSignIn: 'VISIT_AND_SIGN_IN',
}