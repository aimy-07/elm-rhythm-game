import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {csvSetUpSubscriber} from './js/csv'
import {audioSetUpSubscriber} from './js/audio'
import {animationSetUpSubscriber} from './js/animation'



const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();



/* ---------------------------------
	Subscriber
---------------------------------- */
csvSetUpSubscriber(app);
audioSetUpSubscriber(app);
animationSetUpSubscriber(app);
