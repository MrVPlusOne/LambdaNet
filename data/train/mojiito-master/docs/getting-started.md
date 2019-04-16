# Getting Started

## Install
The latest release of Mojiito can be installed using npm.   
Run `npm install mojiito-core mojiito-platform-browser --save` in your command line tool.

## Quickstart

Create you first Component. This component is the start of your application and will be used for bootstrapping.

```typescript
import {Component} from 'mojiito-core';

@Component({
  selector: 'body'
})
export class AppComponent {
}
```

```typescript
import {plattformBrowser} from 'mojiito-platform-browser';
import {AppComponent} from 'app.component.ts';

platformBrowser().bootstrapComponent(AppComponent);
```

For a better structure we recommend splitting components and the code for bootstrapping into different files. Starting with those two:

| Filename  | Description |
| :--- | :--- |
| `main.ts`  | Contains the method for bootstrapping the Mojiito application. Needs to import the app component.  |
| `app.component.ts`  | Top level component. Used for bootstrapping the application. All other components are declared in this or a sub component  |

## Next Chapter
[→ Components](components.md)
