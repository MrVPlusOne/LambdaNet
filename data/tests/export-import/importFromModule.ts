declare module MA{
  import { x } from "MB";
}

declare module "MB" {
  export const x = 4;
}