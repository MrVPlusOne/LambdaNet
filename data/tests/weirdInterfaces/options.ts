import { Instance } from "./instance";
import { getWeek } from "../utils/dates";
import { CustomLocale, key as LocaleKey } from "./locale";

export type DateOption = Date | string | number;
export type DateRangeLimit<D = DateOption> = { from: D; to: D };
export type DateLimit<D = DateOption> =
  | D
  | DateRangeLimit<D>
  | ((date: Date) => boolean);

export type Hook = (
  dates: Date[],
  currentDateString: string,
  self: Instance,
  data?: any
) => void;


export type Plugin<E = {}> = (fp: Instance & E) => Options;

export interface BaseOptions {
  /*
  Allows the user to enter a date directly input the input field. By default, direct entry is disabled.
  */
  allowInput: boolean;

  /* Exactly the same as date format, but for the altInput field */
  altFormat: string;


  /* Disables all dates except these specified. See https://chmln.github.io/flatpickr/examples/#disabling-all-dates-except-select-few */
  enable: DateLimit<DateOption>[];

  /* Enables seconds selection in the time picker.
 */

  /* The locale, either as a string (e.g. "ru", "en") or as an object.
  See https://chmln.github.io/flatpickr/localization/ */
  locale: LocaleKey | CustomLocale;

  /* The maximum date that a user can pick to (inclusive). */
  maxDate: DateOption;

  /* The maximum time that a user can pick to (inclusive). */
  maxTime: DateOption;

  /* The minimum date that a user can start picking from (inclusive). */
  minDate: DateOption;

  /* The minimum time that a user can start picking from (inclusive). */
  minTime: DateOption;


  /* Fires before the calendar instance is destroyed */
  onDestroy: Hook | Hook[];

  /* Fires when valid keyboard input for calendar is detected */
  onKeyDown: Hook | Hook[];

  /* Fires after the month has changed */
  onMonthChange: Hook | Hook[];

  /* Fires after the calendar is opened */
  onOpen: Hook | Hook[];

  /* Fires after the configuration for the calendar is parsed */
  onParseConfig: Hook | Hook[];

  /* Fires once the calendar instance is ready */
  onReady: Hook | Hook[];

  /* Like onChange, but fires immediately after any date changes */
  onValueUpdate: Hook | Hook[];

  /* Fires after the year has changed */
  onYearChange: Hook | Hook[];



  /* HTML for the left arrow icon, used to switch months. */
  prevArrow: string;

  /* Whether to display the current month name in shorthand mode, e.g. "Sep" instead "September" */
  shorthandCurrentMonth: boolean;

  /* Creates a wrapper to position the calendar. Use this if the input is inside a scrollable element */
  static: boolean;

  showMonths?: number;

  /* Displays time picker in 24 hour mode without AM/PM selection when enabled.*/
  time_24hr: boolean;

  /* Display week numbers left of the calendar. */
  weekNumbers: boolean;

  /* See https://chmln.github.io/flatpickr/examples/#flatpickr-external-elements */
  wrap: boolean;
}

export type Options = Partial<BaseOptions>;

export interface ParsedOptions {
  _disable: DateLimit<Date>[];
  _enable: DateLimit<Date>[];
  _maxDate?: Date;
  _maxTime?: Date;
  enable: DateLimit<Date>[];
  enableSeconds: boolean;
  enableTime: boolean;
  errorHandler: (err: Error) => void;
  formatDate?: Options["formatDate"];
  getWeek: (date: Date) => string | number;
  hourIncrement: number;
  ignoredFocusElements: HTMLElement[];
  inline: boolean;
  locale: LocaleKey | CustomLocale;
  maxDate?: Date;
  maxTime?: Date;
  plugins: Plugin[];
  position: BaseOptions["position"];
  positionElement?: HTMLElement;
  prevArrow: string;
  shorthandCurrentMonth: boolean;
  showMonths: number;
  static: boolean;
  time_24hr: boolean;
  weekNumbers: boolean;
  wrap: boolean;
}

