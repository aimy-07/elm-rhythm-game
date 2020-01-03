import firebase from 'firebase/app';
import 'firebase/database';
import {detectedError} from '../index';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Subscriber
---------------------------------- */
export function firebaseDBSetUpSubscriber (app) {
  const firebaseDBGet = (path) => firebase.database().ref(path).once('value');
  const firebaseDBSet = (path, data) => firebase.database().ref(path).set(data, detectedError);

  /* ---------------------------------
	  GET
  ---------------------------------- */
  app.ports.getOwnRecord.subscribe(({uid, csvFileName}) => {
    firebaseDBGet(`/userDatas/${uid}/playRecords/${csvFileName}`)
      .then((snapshot) => {
        const ownRecordDto = {csvFileName, uid, playRecord: snapshot.val()};
        app.ports.gotOwnRecord.send(ownRecordDto);
      })
      .catch(detectedError)
  })

  app.ports.getPublicRecord.subscribe((csvFileName) => {
    firebaseDBGet(`/publicRecords/${csvFileName}/`)
      .then((snapshot) => {
        const publicRecordDto = {csvFileName, bestRecords: snapshot.val()};
        app.ports.gotPublicRecord.send(publicRecordDto);
      })
      .catch(detectedError)
  })

  app.ports.getUsers.subscribe(uidList => {
    const getUsers = uidList.map(uid => firebaseDBGet(`/users/${uid}`));
    Promise.all(getUsers)
      .then(snapshots => {
        // 不正なuserは除く
        const users = snapshots.map(snapshot => snapshot.val());
        const userDtos = users.filter(user => user && user.uid && user.userName && user.pictureUrl);
        app.ports.gotUsers.send(userDtos);
      })
      .catch(detectedError)
  })

  app.ports.getUserSetting.subscribe(uid => {
    firebaseDBGet(`/userDatas/${uid}/settings`)
      .then((snapshot) => {
        if (!snapshot.val()) {
          app.ports.gotUserSetting.send(null);
          return;
        }
        const userSettingDto = {
          currentMusicId: snapshot.val().currentMusicId ? snapshot.val().currentMusicId : null,
          currentMode: snapshot.val().currentMode ? snapshot.val().currentMode : null,
          notesSpeed: snapshot.val().notesSpeed ? snapshot.val().notesSpeed : null,
          bgmVolume: snapshot.val().bgmVolume ? snapshot.val().bgmVolume : null,
          seVolume: snapshot.val().seVolume ? snapshot.val().seVolume : null
        }
        app.ports.gotUserSetting.send(userSettingDto);
      })
      .catch(detectedError)
  });

  /* ---------------------------------
	  POST
  ---------------------------------- */
  app.ports.saveRecord_.subscribe(record => {
    const recordId = uuidv4();
    firebaseDBSet(`/records/${record.csvFileName}/${record.uid}/${recordId}/`, record)
      .then(() => {
        app.ports.savedRecord.send(null);
      })
      .catch(detectedError)
  })

  app.ports.saveOwnRecord_.subscribe(({uid, csvFileName, playRecord}) => {
    firebaseDBSet(`/userDatas/${uid}/playRecords/${csvFileName}`, playRecord)
      .then(() => {
        app.ports.savedOwnRecord.send(null);
      })
      .catch(detectedError)
  })

  app.ports.savePublicRecord_.subscribe(({csvFileName, bestRecords}) => {
    firebaseDBSet(`/publicRecords/${csvFileName}/`, bestRecords)
      .then(() => {
        app.ports.savedPublicRecord.send(null);
      })
      .catch(detectedError)
  })

  app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMusicId/`, currentMusicId)
      .then(() => {})
      .catch(detectedError)
  });

  app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMode/`, currentMode)
      .then(() => {})
      .catch(detectedError)
  });

  app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/notesSpeed/`, notesSpeed)
      .then(() => {})
      .catch(detectedError)
  });

  app.ports.saveBgmVolume.subscribe(({uid, bgmVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/bgmVolume/`, bgmVolume)
      .then(() => {})
      .catch(detectedError)
  });

  app.ports.saveSeVolume.subscribe(({uid, seVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/seVolume/`, seVolume)
      .then(() => {})
      .catch(detectedError)
  });
}