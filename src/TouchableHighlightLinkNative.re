[@bs.module "src/TouchableHighlightLinkNative"] [@react.component]
external make:
  (
    ~href: string,
    ~style: ReactNative.Style.t=?,
    ~activeOpacity: float=?,
    ~underlayColor: string=?,
    ~onPress: ReactEvent.Mouse.t => unit,
    ~children: React.element
  ) =>
  React.element =
  "default";
