import { platformBrowser } from 'mojiito-platform-browser';
import { AppComponent } from './app/app.component';

console.time('bootstrap');
platformBrowser().bootstrapComponent(AppComponent);
console.timeEnd('bootstrap');
