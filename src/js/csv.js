/* ---------------------------------
	定数
---------------------------------- */
const perfectScore = 2000;
const longScore = 100;
const longScoreDuration = 200;

const csvFileNameList = [
  "sample_sound-normal",
  "sample_sound-hard",
  "sample_sound-master",
]



/* ---------------------------------
	Subscriber
---------------------------------- */
export function csvSetUpSubscriber (app) {
  // 全ての楽曲の情報をCSVから取得する
  app.ports.getAllMusicInfoList.subscribe(() => {
    const promises = csvFileNameList.map((csvFileName) => {
      return new Promise((resolve) => {
        const xhr = new XMLHttpRequest();
        xhr.open("GET", `./csv/${csvFileName}.csv`, true);
        xhr.onload = () => {
          const csvArray = createArray(xhr.responseText);
          resolve(getMusicInfo(csvFileName, csvArray));
        };
        xhr.onerror = (e) => {
          console.error(e);
        };
        xhr.send(null);
      })
    })
    Promise.all(promises).then((allMusicInfoList) => {
      console.log("allMusicInfoList", allMusicInfoList);
      app.ports.gotAllMusicInfoList.send(allMusicInfoList);
    });
  })

  // プレイ楽曲の楽曲情報をCSVから取得する
  app.ports.getPlayingMusicInfo.subscribe((csvFileName) => {
    if (!csvFileNameList.includes(csvFileName)) {
      console.error("invalid csvFileName.");
      location.href = "/";
      return;
    }
    const promise = new Promise((resolve) => {
      const xhr = new XMLHttpRequest();
      xhr.open("GET", `./csv/${csvFileName}.csv`, true);
      xhr.onload = () => {
        const csvArray = createArray(xhr.responseText);
        resolve(getMusicInfo(csvFileName, csvArray));
      };
      xhr.onerror = (e) => {
        console.error(e);
      };
      xhr.send(null);
    });
    promise.then((playingMusicInfo) => {
      console.log("playingMusicInfo", playingMusicInfo);
      app.ports.gotPlayingMusicInfo.send(playingMusicInfo);
    });
  })

  // プレイ楽曲のノーツ情報をCSVから取得する
  app.ports.getAllNotes.subscribe((csvFileName) => {
    if (!csvFileNameList.includes(csvFileName)) {
      console.error("invalid url.");
      location.href = "/";
      return;
    }
    const promise = new Promise((resolve) => {
      const xhr = new XMLHttpRequest();
      xhr.open("GET", `./csv/${csvFileName}.csv`, true);
      xhr.onload = () => {
        const csvArray = createArray(xhr.responseText);
        resolve(getAllNotes(csvArray));
      };
      xhr.onerror = (e) => {
        console.error(e);
      };
      xhr.send(null);
    });
    promise.then((allNotes) => {
      console.log("allNotes", allNotes);
      app.ports.gotAllNotes.send(allNotes);
    });
  })
}



const getMusicInfo = (csvFileName, csvArray) => {
  const musicName = csvArray[0][0];
  const composer = csvArray[1][0];
  const mode = csvArray[2][0];
  const level = parseInt(csvArray[3][0], 10);
  const bpm = parseFloat(csvArray[4][0]);
  let maxCombo = 0;
  let maxScore = 0;

  const timePerBeat = 60 * 1000 / bpm;
  csvArray.splice(0, 7);
  csvArray.forEach((csvRow) => {
    csvRow.splice(0, 2);
    csvRow.forEach((note) => {
      if (note && !isNaN(parseInt(note, 10))) {
        const longTime = (parseInt(note, 10) === 0) ? 0 : parseFloat(note) * timePerBeat;
        maxCombo += 1;
        maxScore += perfectScore;
        if (longTime > 0) {
          const longTimeCount = Math.floor(longTime / longScoreDuration);
          maxCombo += 1 * longTimeCount;
          maxScore += longScore * longTimeCount;
        }
      }
    });
  });

  const musicAudio = new Audio();
  const audioName = csvFileName.split("-")[0];
  musicAudio.src = `./audios/${audioName}.wav`;
  musicAudio.load();
  const promise = new Promise((resolve) => {
    musicAudio.addEventListener('loadedmetadata', (e) => {
      const fullTime = musicAudio.duration * 1000;
      const musicInfo = {
        csvFileName,
        musicName,
        composer,
        mode,
        level,
        bpm,
        maxCombo,
        maxScore,
        fullTime,
      }
      resolve(musicInfo);
    });
  });
  return promise.then((musicInfo) => {
    console.log("musicInfo", musicInfo);
    return musicInfo;
  });
}

const getAllNotes = (csvArray) => {
  const bpm = parseFloat(csvArray[4][0]);
  const beatPerMeasure = parseFloat(csvArray[5][0]);
  const offset = parseFloat(csvArray[6][0]);
  const timePerBeat = 60 * 1000 / bpm;

  csvArray.splice(0, 7);
  const allNotes_ = csvArray.map((csvRow) => {
    const measure = parseFloat(csvRow[0]);
    const beat = parseFloat(csvRow[1]);
    const justTime = (measure * beatPerMeasure + beat) * timePerBeat + offset * 1000;

    csvRow.splice(0, 2);
    const notes = csvRow.map((note) => {
      if (note && !isNaN(parseInt(note, 10))) {
        const longTime = (parseInt(note, 10) === 0) ? 0 : parseFloat(note, 10) * timePerBeat;
        return {justTime, longTime};
      }
      const longTime = -1;
      return {justTime, longTime};
    })
    return notes;
  })
  const tAllNotes = transpose(allNotes_);
  const allNotes = {
    laneS: tAllNotes[0],
    laneD: tAllNotes[1],
    laneF: tAllNotes[2],
    laneJ: tAllNotes[3],
    laneK: tAllNotes[4],
    laneL: tAllNotes[5],
  }
  console.log(allNotes);
  return allNotes;
}

// csvデータを配列に変換する
const createArray = (csvData) => {
  const tempArray = csvData.split("\n");
  const csvArray = new Array();
  for(let i = 0; i < tempArray.length; i++){
    csvArray[i] = tempArray[i].split(",");
  }
  return csvArray
}

// 配列を転置する
const transpose = a => a[0].map((_, c) => a.map(r => r[c]));