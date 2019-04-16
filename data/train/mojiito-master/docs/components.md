# Components

Mojiito is made up of *components*. Components are the basic building blocks and are a combination of an element (with its subtree of nodes) and a javascript class. Let's look at the example below:
```ts
import {Component} from 'mojiito-core';

@Component({
  selector: 'body'
})
export class AppComponent {
}
```

As you can see we have declared a normal javascript class called `AppComponent`. To transform this class into a component we need to it wrap with a `Component` decorator function. This `@Component` function takes a metadata object as a parameter which enhances the class with additional information Mojiito needs create instances.

The most basic metadata object has a `selector` property. Mojiito uses it to finde matching elements in the DOM and creating instances of that component for every found element. Every component needs a least a `selector`, the other metadata properties are optional.
