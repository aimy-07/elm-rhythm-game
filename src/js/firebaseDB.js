import firebase from 'firebase/app';
import 'firebase/database';
import {detectedError, errorEvent} from '../index';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Subscriber
---------------------------------- */
export function firebaseDBSetUpSubscriber (app) {
  const firebaseDBGet = (path) => firebase.database().ref(path).once('value');
  const firebaseDBSet = (path, data) => firebase.database().ref(path).set(data);

  /* ---------------------------------
	  GET
  ---------------------------------- */
  app.ports.getOwnRecord.subscribe(({uid, csvFileName}) => {
    firebaseDBGet(`/userDatas/${uid}/playRecords/${csvFileName}`)
      .then((snapshot) => {
        const ownRecordDto = {csvFileName, uid, playRecord: snapshot.val()};
        app.ports.gotOwnRecord.send(ownRecordDto);
      })
      .catch(error => {
        detectedError(errorEvent.getOwnRecord, error, {uid, csvFileName});
      });
  })

  app.ports.getPublicRecord.subscribe(csvFileName => {
    firebaseDBGet(`/publicRecords/${csvFileName}/`)
      .then((snapshot) => {
        const publicRecordDto = {csvFileName, bestRecords: snapshot.val()};
        app.ports.gotPublicRecord.send(publicRecordDto);
      })
      .catch(error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.getPublicRecord, error, {uid, csvFileName});
      });
  })

  app.ports.getUsers.subscribe(uidList => {
    const getUsers = uidList.map(uid => firebaseDBGet(`/users/${uid}`));
    Promise.all(getUsers)
      .then(snapshots => {
        const users = snapshots.map(snapshot => snapshot.val());
        // 不正なuserは除く
        const userDtos = users.filter(user =>
          isValid(user) && isValid(user.uid) && isValid(user.userName) && isValid(user.pictureUrl)
        );
        app.ports.gotUsers.send(userDtos);
      })
      .catch(error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.getUsers, error, {uid});
      });
  })

  app.ports.getUserSetting.subscribe(uid => {
    firebaseDBGet(`/userDatas/${uid}/settings`)
      .then((snapshot) => {
        if (!snapshot.val()) {
          app.ports.gotUserSetting.send(null);
          return;
        }
        const userSettingDto = {
          currentMusicId: isValid(snapshot.val().currentMusicId) ? snapshot.val().currentMusicId : null,
          currentMode: isValid(snapshot.val().currentMode) ? snapshot.val().currentMode : null,
          notesSpeed: isValid(snapshot.val().notesSpeed) ? snapshot.val().notesSpeed : null,
          bgmVolume: isValid(snapshot.val().bgmVolume) ? snapshot.val().bgmVolume : null,
          seVolume: isValid(snapshot.val().seVolume) ? snapshot.val().seVolume : null
        }
        app.ports.gotUserSetting.send(userSettingDto);
      })
      .catch(error => {
        detectedError(errorEvent.getUserSetting, error, {uid});
      });
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
      .catch(error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.saveRecord, error, {uid, record});
      });
  })

  app.ports.saveOwnRecord_.subscribe(({uid, csvFileName, playRecord}) => {
    firebaseDBSet(`/userDatas/${uid}/playRecords/${csvFileName}`, playRecord)
      .then(() => {
        app.ports.savedOwnRecord.send(null);
      })
      .catch(error => {
        detectedError(errorEvent.saveOwnRecord, error, {uid, csvFileName, playRecord});
      });
  })

  app.ports.savePublicRecord_.subscribe(({csvFileName, bestRecords}) => {
    firebaseDBSet(`/publicRecords/${csvFileName}/`, bestRecords)
      .then(() => {
        app.ports.savedPublicRecord.send(null);
      })
      .catch(error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.savePublicRecord, error, {uid, csvFileName, bestRecords});
      });
  })

  app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMusicId/`, currentMusicId)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveCurrentMusicId, error, {uid, currentMusicId});
      });
  });

  app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMode/`, currentMode)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveCurrentMode, error, {uid, currentMode});
      });
  });

  app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/notesSpeed/`, notesSpeed)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveNotesSpeed, error, {uid, notesSpeed});
      });
  });

  app.ports.saveBgmVolume.subscribe(({uid, bgmVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/bgmVolume/`, bgmVolume)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveBgmVolume, error, {uid, bgmVolume});
      });
  });

  app.ports.saveSeVolume.subscribe(({uid, seVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/seVolume/`, seVolume)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveSeVolume, error, {uid, seVolume});
      });
  });
}

const isValid = (data) => {
  if (data !== false && data !== null && data !== undefined && data !== NaN && data !== '' ) {
    return true;
  } else {
    return false;
  }
}