import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import 'firebase/storage';
import {detectedError} from '../index';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Provider
---------------------------------- */
const googleAuthProvider = new firebase.auth.GoogleAuthProvider();
const twitterAuthProvider = new firebase.auth.TwitterAuthProvider();
const githubAuthProvider = new firebase.auth.GithubAuthProvider();



/* ---------------------------------
	Subscriber
---------------------------------- */
export function authSetUpSubscriber (app) {
  // サインイン
  app.ports.signInWithGoogle.subscribe(() => {
    firebase.auth().signInWithPopup(googleAuthProvider)
      .then(() => {})
      .catch((error) => {
        app.ports.canceledSignIn.send(null);
      });
  });

  app.ports.signInWithTwitter.subscribe(() => {
    firebase.auth().signInWithPopup(twitterAuthProvider)
      .then(() => {})
      .catch((error) => {
        app.ports.canceledSignIn.send(null);
      });
  });

  app.ports.signInWithGithub.subscribe(() => {
    firebase.auth().signInWithPopup(githubAuthProvider)
      .then(() => {})
      .catch((error) => {
        app.ports.canceledSignIn.send(null);
      });
  });

  // サインアウト
  app.ports.signOut.subscribe(() => {
    firebase.auth().signOut()
      .then(() => {})
      .catch(detectedError);
  });

  // ユーザーネームの変更
  app.ports.saveUserName.subscribe(({uid, userName}) => {
    const currentUser = firebase.auth().currentUser;
    if (currentUser && currentUser.uid === uid) {
      const updateAuth = currentUser.updateProfile({displayName: userName});
      const updateDB = firebase.database().ref(`/users/${uid}/userName/`).set(userName, detectedError);
      Promise.all([updateAuth, updateDB])
        .then(() => {})
        .catch(detectedError)
    } else {
      detectedError('currentUser.uid !== uid')
    }
  });

  // ユーザーアイコンの変更
  app.ports.saveUserPicture.subscribe(({uid, event}) => {
    const file = event.target.files[0];
    const fileType = getFileType(file.name);
    if (fileType === "") {
      detectedError('fileType in not JPG or PNG');
      return;
    }
    const filePath = `userIcons/icon_${uid}.${fileType}`
    const uploadFile = () => firebase.storage().ref().child(filePath).put(file);
    const getNewPictureUrl = () => firebase.storage().ref(filePath).getDownloadURL()

    uploadFile()
      .then(getNewPictureUrl)
      .then(url => {
        const currentUser = firebase.auth().currentUser;
        if (currentUser && currentUser.uid === uid) {
          const updateAuth = currentUser.updateProfile({photoURL: url});
          const updateDB = firebase.database().ref(`/users/${uid}/pictureUrl/`).set(url, detectedError);
          Promise.all([updateAuth, updateDB])
            .then(() => {
              app.ports.savedUserPicture.send(url)
            })
            .catch(detectedError)
        } else {
          detectedError('currentUser.uid !== uid')
        }
      })
      .catch(detectedError)
  });
}



/* ---------------------------------
	拡張子のチェック
---------------------------------- */
const getFileType = (fileName) => {
  const type = fileName.split('.');
  const typeStr = type[type.length - 1].toLowerCase();
  if (typeStr == 'png') {
    return "png";
  } else if (typeStr == 'jpg') {
    return "jpg";
  } else if (typeStr == 'jpeg') {
    return "jpeg";
  }
  return ""
}