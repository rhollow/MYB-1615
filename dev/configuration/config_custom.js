var sakai = sakai || {};

sakai.config = sakai.config || {};

// Insert custom configuration here

sakai.config.Navigation = [
    {
        "url" : "/dev/my_sakai.html",
        "label" : "MY_SAKAI"
    }
];

// This is our custom CAS log in information. Please note, we're not actually using the external object yet 

sakai.config.Authentication = {
    "internal": sakai.isDev, 
    "external": [
        {
          label: "LOGIN_TITLE",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/dev/index.html",
          description: "CAS_NOTE"
        }
    ]
};
