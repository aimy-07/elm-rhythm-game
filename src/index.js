import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {firebaseAuthSetUpSubscriber} from './js/firebaseAuth';
import {firebaseDBSetUpSubscriber} from './js/firebaseDB';
import {dataSetUpSubscriber} from './js/data';
import {audioSetUpSubscriber, BGM, SE} from './js/audio';
import {animationSetUpSubscriber} from './js/animation';
import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import 'firebase/analytics';
import {firebaseConfig} from './config';



/* ---------------------------------
  Elm init
---------------------------------- */
const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();



/* ---------------------------------
  Firebase init
---------------------------------- */
const firebaseApp = firebase.initializeApp(firebaseConfig);



/* ---------------------------------
  onAuthStateChanged
---------------------------------- */
firebase.auth().onAuthStateChanged((user) => {
  if (!user) {
    // サインインしていない状態
    app.ports.onAuthStateChanged.send(null);
  } else {
    // サインイン済み
    const userInfo = {
      uid: user.uid,
      userName: user.displayName,
      pictureUrl: user.photoURL
    }
    firebase.database().ref(`/users/${user.uid}`).set(userInfo)
      .then(() => {
        app.ports.onAuthStateChanged.send(userInfo);
      })
      .catch(error => {
        detectedError(errorEvent.setUserInfoOnAuthChanged, error, userInfo);
      })
  }
});



/* ---------------------------------
  Subscriber
---------------------------------- */
firebaseAuthSetUpSubscriber(app);
firebaseDBSetUpSubscriber(app);
dataSetUpSubscriber(app);
audioSetUpSubscriber(app, BGM(app), SE(app));
animationSetUpSubscriber(app);



/* ---------------------------------
  エラーログ
---------------------------------- */
const firebaseAnalytics = firebase.analytics(firebaseApp);

export const errorEvent = {
  signIn: 'sign_in',
  signOut: 'sign_out',
  setUserInfoOnAuthChanged: 'set_user_info_on_auth_changed',
  saveUserName: 'save_user_name',
  saveUserPicture: 'save_user_picture',
  loadMusicDataByJson: 'load_music_data_by_json',
  loadMusicDataByCsv: 'load_music_data_by_csv',
  getOwnRecord: 'get_own_record',
  getPublicRecord: 'get_public_record',
  getUsers: 'get_users',
  getUserSetting: 'get_user_setting',
  saveRecord: 'save_record',
  saveOwnRecord: 'save_own_record',
  savePublicRecord: 'save_public_record',
  saveCurrentMusicId: 'save_current_musicId',
  saveCurrentMode: 'save_current_mode',
  saveNotesSpeed: 'save_notes_speed',
  saveBgmVolume: 'save_bgm_volume',
  saveSeVolume: 'save_se_volume'
}



/* ---------------------------------
  エラー処理
---------------------------------- */
export const detectedError = (event, error, eventProperty) => {
  if (error) {
    console.error(event, error);
    const time = new Date();
    firebaseAnalytics.logEvent(event, Object.assign({error, time}, eventProperty));
    app.ports.detectedError.send(null);
  }
}